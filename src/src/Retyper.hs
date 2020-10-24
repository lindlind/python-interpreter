{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Retyper 
  ( tfParse
  ) where

import ClassDef
  ( IExpr (..)
  , IStatement (..)
  , IPyScript
  , IPyType
  , ParseException (..)
  , def
  )
import Lexer
  ( Token (..)
  )
import Parser 
  ( ExprParse (..)
  , StatementParse (..)
  , parse
  )

import Control.Applicative
  ( (<|>)
  )
import Control.Monad.Except 
  ( ExceptT
  , runExceptT
  , throwError
  )
import Control.Monad.State.Strict 
  ( State
  , get
  , modify
  , runState
  , put
  )
import Data.Maybe
  ( isJust
  )
import qualified Data.Map.Strict as Map
  ( Map
  , (!?)
  , empty
  , insert
  )
import Data.Typeable
  ( (:~:) (Refl)
  , TypeRep
  , eqT
  , typeOf
  )

data RetyperEnvironment 
  = RetEnvStmt 
  { oldStmt :: StatementParse
  , varsMap :: Map.Map String TypeRep
  , funcsMap :: Map.Map String ([TypeRep], TypeRep)
  }
  | RetEnvExpr
  { oldExpr :: ExprParse
  , varsMap :: Map.Map String TypeRep
  , funcsMap :: Map.Map String ([TypeRep], TypeRep)
  }

initRetyperEnv :: StatementParse -> RetyperEnvironment
initRetyperEnv statements 
  = RetEnvStmt 
  { oldStmt = statements
  , varsMap = Map.empty
  , funcsMap = Map.empty
  }

type RetyperMonad = State RetyperEnvironment

type ExcRetyperMonad = ExceptT ParseException RetyperMonad

runRetyper :: ExcRetyperMonad a 
              -> RetyperEnvironment 
              -> (Either ParseException a, RetyperEnvironment)                
runRetyper = runState . runExceptT

data Encaps stmt where
  Encaps :: (IExpr expr, IPyType t) => expr t -> Encaps expr

fromEncapsBool :: Encaps expr -> Maybe (expr Bool)
fromEncapsBool (Encaps (enc :: expr t)) = do
  Refl <- eqT @t @Bool
  return enc

fromEncapsInt :: Encaps expr -> Maybe (expr Integer)
fromEncapsInt (Encaps (enc :: expr t)) = do
  Refl <- eqT @t @Integer
  return enc

fromEncapsFloat :: Encaps expr -> Maybe (expr Double)
fromEncapsFloat (Encaps (enc :: expr t)) = do
  Refl <- eqT @t @Double
  return enc

fromEncapsStr :: Encaps expr -> Maybe (expr String)
fromEncapsStr (Encaps (enc :: expr t)) = do
  Refl <- eqT @t @String
  return enc

modifyEnvExpr :: ExprParse -> RetyperEnvironment -> RetyperEnvironment
modifyEnvExpr newA env@RetEnvExpr{ oldExpr = _ } = env { oldExpr = newA }
modifyEnvExpr _ _ = error "modifyEnvExpr: impossible constructor"

modifyEnvStmt :: StatementParse -> RetyperEnvironment -> RetyperEnvironment
modifyEnvStmt newA env@RetEnvStmt{ oldStmt = _ } = env { oldStmt = newA }
modifyEnvStmt _ _ = error "modifyEnvStmt: impossible constructor"

modifyEnvMapVars :: (Map.Map String TypeRep -> Map.Map String TypeRep)
                    -> RetyperEnvironment -> RetyperEnvironment
modifyEnvMapVars f env@RetEnvStmt{ varsMap = mp } = env { varsMap = f mp }
modifyEnvMapVars _ _ = error "modifyEnvMapVars: impossible constructor"

modifyEnvMapFuncs :: ( Map.Map String ([TypeRep], TypeRep) 
                       -> Map.Map String ([TypeRep], TypeRep)
                     ) -> RetyperEnvironment -> RetyperEnvironment
modifyEnvMapFuncs f env@RetEnvStmt{ funcsMap = mp } = env { funcsMap = f mp }
modifyEnvMapFuncs _ _ = error "modifyEnvMapFuncs: impossible constructor"

retypeVarStr :: IPyScript expr => String -> expr String
retypeVarStr = iVariable

retypeVarInt :: IPyScript expr => String -> expr Integer
retypeVarInt = iVariable

retypeVarFloat :: IPyScript expr => String -> expr Double
retypeVarFloat = iVariable

retypeVarBool :: IPyScript expr => String -> expr Bool
retypeVarBool = iVariable

retypeF0Str :: IPyScript expr => String -> expr String
retypeF0Str = iCallFunc0

retypeF0Int :: IPyScript expr => String -> expr Integer
retypeF0Int = iCallFunc0

retypeF0Float :: IPyScript expr => String -> expr Double
retypeF0Float = iCallFunc0

retypeF0Bool :: IPyScript expr => String -> expr Bool
retypeF0Bool = iCallFunc0

retypeF1Str :: IPyScript expr => String -> expr () -> expr String
retypeF1Str = iCallFunc1

retypeF1Int :: IPyScript expr => String -> expr () -> expr Integer
retypeF1Int = iCallFunc1

retypeF1Float :: IPyScript expr => String -> expr () -> expr Double
retypeF1Float = iCallFunc1

retypeF1Bool :: IPyScript expr => String -> expr () -> expr Bool
retypeF1Bool = iCallFunc1

retypeF2Str :: IPyScript expr => String -> expr () -> expr () -> expr String
retypeF2Str = iCallFunc2

retypeF2Int :: IPyScript expr => String -> expr () -> expr () -> expr Integer
retypeF2Int = iCallFunc2

retypeF2Float :: IPyScript expr => String -> expr () -> expr () -> expr Double
retypeF2Float = iCallFunc2

retypeF2Bool :: IPyScript expr => String -> expr () -> expr () -> expr Bool
retypeF2Bool = iCallFunc2

typeStrToRep :: String -> TypeRep
typeStrToRep "str"   = typeOf (def :: String)
typeStrToRep "int"   = typeOf (def :: Integer)
typeStrToRep "float" = typeOf (def :: Double)
typeStrToRep "bool"  = typeOf (def :: Bool)
typeStrToRep _ = error "typeStrToRep: impossible case"

correctArgType :: IPyScript expr => TypeRep -> Encaps expr -> Bool
correctArgType rep enc =
  case show rep of
    "[Char]"  -> isJust $ fromEncapsStr   enc
    "Integer" -> isJust $ fromEncapsInt   enc
    "Double"  -> isJust $ fromEncapsFloat enc
    "Bool"    -> isJust $ fromEncapsBool  enc
    _ -> error "correctArgType: impossible case"

tryUnOpStr :: (IPyScript expr, IPyType t) => (expr String -> expr t)
              -> Encaps expr -> Maybe (Encaps expr)
tryUnOpStr unOp enc = do
  j <- fromEncapsStr enc
  return $ Encaps $ unOp j

tryUnOpInt :: (IPyScript expr, IPyType t) => (expr Integer -> expr t)
              -> Encaps expr -> Maybe (Encaps expr)
tryUnOpInt unOp enc = do
  j <- fromEncapsInt enc
  return $ Encaps $ unOp j

tryUnOpFloat :: (IPyScript expr, IPyType t) => (expr Double -> expr t)
                -> Encaps expr -> Maybe (Encaps expr)
tryUnOpFloat unOp enc = do
  j <- fromEncapsFloat enc
  return $ Encaps $ unOp j

tryUnOpBool :: (IPyScript expr, IPyType t) => (expr Bool -> expr t)
               -> Encaps expr -> Maybe (Encaps expr)
tryUnOpBool unOp enc = do
  j <- fromEncapsBool enc
  return $ Encaps $ unOp j

tryBinOpStr :: (IPyScript expr, IPyType t) 
               => (expr String -> expr String -> expr t)
               -> Encaps expr -> Encaps expr -> Maybe (Encaps expr)
tryBinOpStr binOp enc1 enc2 = do
  j1 <- fromEncapsStr enc1
  j2 <- fromEncapsStr enc2
  return $ Encaps $ binOp j1 j2

tryBinOpInt :: (IPyScript expr, IPyType t) 
               => (expr Integer -> expr Integer -> expr t)
               -> Encaps expr -> Encaps expr -> Maybe (Encaps expr)
tryBinOpInt binOp enc1 enc2 = do
  j1 <- fromEncapsInt enc1
  j2 <- fromEncapsInt enc2
  return $ Encaps $ binOp j1 j2

tryBinOpFloat :: (IPyScript expr, IPyType t) 
                 => (expr Double -> expr Double -> expr t)
                 -> Encaps expr -> Encaps expr -> Maybe (Encaps expr)
tryBinOpFloat binOp enc1 enc2 = do
  j1 <- ( fromEncapsInt enc1 >>= \int -> return (iHidCastFloat int) ) 
        <|> fromEncapsFloat enc1
  j2 <- ( fromEncapsInt enc2 >>= \int -> return (iHidCastFloat int) ) 
        <|> fromEncapsFloat enc2
  return $ Encaps $ binOp j1 j2

tryBinOpBool :: (IPyScript expr, IPyType t) 
                => (expr Bool -> expr Bool -> expr t)
                -> Encaps expr -> Encaps expr -> Maybe (Encaps expr)
tryBinOpBool binOp enc1 enc2 = do
  j1 <- fromEncapsBool enc1
  j2 <- fromEncapsBool enc2
  return $ Encaps $ binOp j1 j2

stmtRetyper :: IPyScript stmt => ExcRetyperMonad (stmt ())
stmtRetyper = do
  env <- get
  let (RetEnvStmt old mpVars mpFuncs) = env
  case old of
    IfWT ifPos e a -> do
      case (runRetyper exprRetyper $ RetEnvExpr e mpVars mpFuncs) of
        (Left err, _) -> throwError err
        (Right enc, _) -> case (fromEncapsBool enc) of
          (Just cond) -> do
            modify $ modifyEnvStmt a
            thn <- stmtRetyper
            return (iIf cond thn)
          _ -> throwError $ TypeError ["bool"] "if statement" ifPos

    IfElseWT ifPos e a b -> do
      case (runRetyper exprRetyper $ RetEnvExpr e mpVars mpFuncs) of
        (Left err, _) -> throwError err
        (Right enc, _) -> case (fromEncapsBool enc) of
          (Just cond) -> do
            modify $ modifyEnvStmt a
            thn <- stmtRetyper
            modify $ modifyEnvStmt b
            els <- stmtRetyper
            return (iIfElse cond thn els)
          _ -> throwError $ TypeError ["bool"] "if statement" ifPos

    WhileWT whilePos e a -> do
      case (runRetyper exprRetyper $ RetEnvExpr e mpVars mpFuncs) of
        (Left err, _) -> throwError err
        (Right enc, _) -> case (fromEncapsBool enc) of
          (Just cond) -> do
            modify $ modifyEnvStmt a
            thn <- stmtRetyper
            return (iWhile cond thn)
          _ -> throwError $ TypeError ["bool"] "while statement" whilePos

    BreakWT -> return iBreak

    ContinueWT -> return iContinue

    DefFunc0WT (TVariable _ _ fName) 
               (TType _ _ resultType) a -> do
      case mpFuncs Map.!? fName of
        (Just _) -> throwError $ FunctionRedefinitionError fName
        Nothing -> do
          let listArgs = []
          modify $ modifyEnvMapFuncs 
            (Map.insert fName (listArgs, typeStrToRep resultType))
          modify $ modifyEnvStmt a
          body <- stmtRetyper
          put env
          modify $ modifyEnvMapFuncs 
            (Map.insert fName (listArgs, typeStrToRep resultType))
          return (iDefFunc0 fName body)
    DefFunc0WT _ _ _ -> error "stmtRetyper.DefFunc0WT: impossible case"

    DefFunc1WT (TVariable _ _ fName)
               (TVariable _ _ argName) (TType _ _ argType)
               (TType _ _ resultType) a -> do
      case mpFuncs Map.!? fName of
        (Just _) -> throwError $ FunctionRedefinitionError fName
        Nothing -> do
          let listArgs = [typeStrToRep argType]
          modify $ modifyEnvMapVars
            (Map.insert argName $ typeStrToRep argType)
          modify $ modifyEnvMapFuncs 
            (Map.insert fName (listArgs, typeStrToRep resultType))
          modify $ modifyEnvStmt a
          body <- stmtRetyper
          put env
          modify $ modifyEnvMapFuncs 
            (Map.insert fName (listArgs, typeStrToRep resultType))
          return (iDefFunc1 fName argName body)
    DefFunc1WT _ _ _ _ _ -> error "stmtRetyper.DefFunc1WT: impossible case"

    DefFunc2WT (TVariable _ _ fName)
               (TVariable _ _ arg1Name) (TType _ _ arg1Type)
               (TVariable _ _ arg2Name) (TType _ _ arg2Type)
               (TType _ _ resultType) a -> do
      case mpFuncs Map.!? fName of
        (Just _) -> throwError $ FunctionRedefinitionError fName
        Nothing -> do
          let listArgs = [typeStrToRep arg1Type, typeStrToRep arg2Type]
          modify $ modifyEnvMapVars 
            (Map.insert arg1Name $ typeStrToRep arg1Type)      
          modify $ modifyEnvMapVars 
            (Map.insert arg2Name $ typeStrToRep arg2Type)
          modify $ modifyEnvMapFuncs 
            (Map.insert fName (listArgs, typeStrToRep resultType))
          modify $ modifyEnvStmt a
          body <- stmtRetyper
          put env
          modify $ modifyEnvMapFuncs 
            (Map.insert fName (listArgs, typeStrToRep resultType))
          return (iDefFunc2 fName arg1Name arg2Name body)
    DefFunc2WT _ _ _ _ _ _ _ -> error "stmtRetyper.DefFunc2WT: impossible case"

    ReturnWT e -> do
      case (runRetyper exprRetyper $ RetEnvExpr e mpVars mpFuncs) of
        (Left err, _) -> throwError err
        (Right (Encaps j), _) -> return (iReturn j)

    AssignWT (TVariable _ posAssign vName) e -> do
      case (runRetyper exprRetyper $ RetEnvExpr e mpVars mpFuncs) of
        (Left err, _) -> throwError err
        (Right enc, _) -> do
          let str   = fromEncapsStr   enc
          let int   = fromEncapsInt   enc
          let float = fromEncapsFloat enc
          let bool  = fromEncapsBool  enc
          case (str, int, float, bool) of
            (Just j, _, _, _) -> do
              modify $ modifyEnvMapVars 
                (Map.insert vName $ typeOf (def :: String))
              return (iAssign vName j)        
            (_, Just j, _, _) -> do
              modify $ modifyEnvMapVars 
                (Map.insert vName $ typeOf (def :: Integer))
              return (iAssign vName j)
            (_, _, Just j, _) -> do
              modify $ modifyEnvMapVars 
                (Map.insert vName $ typeOf (def :: Double))
              return (iAssign vName j)
            (_, _, _, Just j) -> do
              modify $ modifyEnvMapVars 
                (Map.insert vName $ typeOf (def :: Bool))
              return (iAssign vName j)
            _ -> throwError $ TypeError 
                                ["str", "int", "float", "bool"] 
                                "assign statement" 
                                posAssign
    AssignWT _ _ -> error "stmtRetyper.AssignWT: impossible case"

    ProcedureWT e -> do
      case (runRetyper exprRetyper $ RetEnvExpr e mpVars mpFuncs) of
        (Left err, _) -> throwError err
        (Right (Encaps j), _) -> return (iProcedure j)

    PrintWT e -> do
      case (runRetyper exprRetyper $ RetEnvExpr e mpVars mpFuncs) of
        (Left err, _) -> throwError err
        (Right (Encaps j), _) -> return (iPrint j)

    StmtPrsSeq a b -> do
      modify $ modifyEnvStmt a
      r1 <- stmtRetyper
      modify $ modifyEnvStmt b
      r2 <- stmtRetyper
      return (iNextStmt r1 r2)

    EmptySeq -> throwError $ CommonParserError "Syntax error: empty file"

exprRetyper :: IPyScript expr => ExcRetyperMonad (Encaps expr)
exprRetyper = do
  env <- get
  let (RetEnvExpr old mpVars mpFuncs) = env
  case old of
    BinOpWT a1 token a2 -> do
      modify $ modifyEnvExpr a1
      enc1 <- exprRetyper
      modify $ modifyEnvExpr a2
      enc2 <- exprRetyper
      case content token of
        ("or") -> do
          let res = tryBinOpBool iOr enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("and") -> do
          let res = tryBinOpBool iAnd enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)

        ("==") -> do
          let res = tryBinOpStr   iEq enc1 enc2 
                <|> tryBinOpInt   iEq enc1 enc2 
                <|> tryBinOpFloat iEq enc1 enc2 
                <|> tryBinOpBool  iEq enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("!=") -> do
          let res = tryBinOpStr   iNEq enc1 enc2 
                <|> tryBinOpInt   iNEq enc1 enc2 
                <|> tryBinOpFloat iNEq enc1 enc2 
                <|> tryBinOpBool  iNEq enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("<") -> do
          let res = tryBinOpStr   iLT enc1 enc2 
                <|> tryBinOpInt   iLT enc1 enc2 
                <|> tryBinOpFloat iLT enc1 enc2 
                <|> tryBinOpBool  iLT enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        (">") -> do
          let res = tryBinOpStr   iGT enc1 enc2 
                <|> tryBinOpInt   iGT enc1 enc2 
                <|> tryBinOpFloat iGT enc1 enc2 
                <|> tryBinOpBool  iGT enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("<=") -> do
          let res = tryBinOpStr   iLTE enc1 enc2 
                <|> tryBinOpInt   iLTE enc1 enc2 
                <|> tryBinOpFloat iLTE enc1 enc2 
                <|> tryBinOpBool  iLTE enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        (">=") -> do
          let res = tryBinOpStr   iGTE enc1 enc2 
                <|> tryBinOpInt   iGTE enc1 enc2 
                <|> tryBinOpFloat iGTE enc1 enc2 
                <|> tryBinOpBool  iGTE enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)

        ("|") -> do
          let res = tryBinOpInt iBitOr enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("^") -> do
          let res = tryBinOpInt iBitXor enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("&") -> do
          let res = tryBinOpInt iBitAnd enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("<<") -> do
          let res = tryBinOpInt iLeftShift enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        (">>") -> do
          let res = tryBinOpInt iRightShift enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)

        ("+") -> do
          let res = tryBinOpInt    iPlus enc1 enc2 
                <|> tryBinOpFloat  iPlus enc1 enc2 
                <|> tryBinOpStr iStrPlus enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("-") -> do
          let res = tryBinOpInt   iMinus enc1 enc2 
                <|> tryBinOpFloat iMinus enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("*") -> do
          let res = tryBinOpInt   iMult enc1 enc2 
                <|> tryBinOpFloat iMult enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("/") -> do
          let res = tryBinOpFloat iFloatDiv enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("//") -> do
          let res = tryBinOpInt iDiv enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("%") -> do
          let res = tryBinOpInt iMod enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        ("**") -> do
          let res = tryBinOpFloat iPow enc1 enc2
          case res of
            Just j -> return j
            _ -> throwError $ OpTypeError (content token) (position token)
        _ -> error "exprRetyper.BinOpWT: impossible case"

    UnOpWT token a -> do
      modify $ modifyEnvExpr a
      enc <- exprRetyper
      let bool  = fromEncapsBool  enc
      let int   = fromEncapsInt   enc
      let float = fromEncapsFloat enc
      case content token of
        ("not") -> case bool of
          (Just j) -> return (Encaps $ iNot j)
          _ -> throwError $ OpTypeError (content token) (position token)
        ("+") -> case (int, float) of
          (Just j, _) -> return (Encaps $ iUnarPlus j)
          (_, Just j) -> return (Encaps $ iUnarPlus j)
          _ -> throwError $ OpTypeError (content token) (position token)
        ("-") -> case (int, float) of
          (Just j, _) -> return (Encaps $ iUnarMinus j)
          (_, Just j) -> return (Encaps $ iUnarMinus j)
          _ -> throwError $ OpTypeError (content token) (position token)
        _ -> error "exprRetyper.UnOpWT: impossible case"

    CastWT token a -> do
      modify $ modifyEnvExpr a
      enc <- exprRetyper
      case content token of
        ("str") -> do
          let res = tryUnOpStr   iCastStr enc <|> tryUnOpInt  iCastStr enc
                <|> tryUnOpFloat iCastStr enc <|> tryUnOpBool iCastStr enc
          case res of
            Just j -> return j
            _ -> throwError $ CastTypeError (position token)
        ("int") -> do
          let res = tryUnOpStr   iCastInt enc <|> tryUnOpInt  iCastInt enc
                <|> tryUnOpFloat iCastInt enc <|> tryUnOpBool iCastInt enc
          case res of
            Just j -> return j
            _ -> throwError $ CastTypeError (position token)
        ("float") -> do
          let res = tryUnOpStr   iCastFloat enc <|> tryUnOpInt  iCastFloat enc
                <|> tryUnOpFloat iCastFloat enc <|> tryUnOpBool iCastFloat enc
          case res of
            Just j -> return j
            _ -> throwError $ CastTypeError (position token)
        ("bool") -> do
          let res = tryUnOpStr   iCastBool enc <|> tryUnOpInt  iCastBool enc
                <|> tryUnOpFloat iCastBool enc <|> tryUnOpBool iCastBool enc
          case res of
            Just j -> return j
            _ -> throwError $ CastTypeError (position token)
        _ -> error "exprRetyper.CastWT: impossible case"

    CallFunc0WT (TVariable _ pos fName) -> do
      case (mpFuncs Map.!? fName) of
        Nothing -> throwError $ FunctionNotDefinedError fName pos
        (Just ([], typeRepResult)) -> do
          case show typeRepResult of
            "[Char]"  -> return (Encaps $ retypeF0Str fName)
            "Integer" -> return (Encaps $ retypeF0Int fName)
            "Double"  -> return (Encaps $ retypeF0Float fName)
            "Bool"    -> return (Encaps $ retypeF0Bool fName)
            _ -> error "exprRetyper.CallFunc0WT.typeResult: impossible case"
        _ -> throwError $ FunctionArgsCountError fName pos
    CallFunc0WT _ -> error "exprRetyper.CallFunc0WT: impossible case"

    CallFunc1WT (TVariable _ pos fName) a -> do
      case (mpFuncs Map.!? fName) of
        Nothing -> throwError $ FunctionNotDefinedError fName pos
        (Just (typeRepArg : [], typeRepResult)) -> do
          modify $ modifyEnvExpr a
          enc <- exprRetyper
          let arg = case enc of 
                      Encaps j -> iPushToStack j
          case correctArgType typeRepArg enc of
            False -> throwError $ FunctionArgsTypeError fName "first" pos
            True -> do
              case show typeRepResult of
                "[Char]"  -> return (Encaps $ retypeF1Str fName arg)
                "Integer" -> return (Encaps $ retypeF1Int fName arg)
                "Double"  -> return (Encaps $ retypeF1Float fName arg)
                "Bool"    -> return (Encaps $ retypeF1Bool fName arg)
                _ -> error "exprRetyper.CallFunc1WT.typeResult: impossible case"
        _ -> throwError $ FunctionArgsCountError fName pos
    CallFunc1WT _ _ -> error "exprRetyper.CallFunc1WT: impossible case"

    CallFunc2WT (TVariable _ pos fName) a b -> do
      case (mpFuncs Map.!? fName) of
        Nothing -> throwError $ FunctionNotDefinedError fName pos
        (Just (typeRepArg1 : typeRepArg2 : [], typeRepResult)) -> do
          modify $ modifyEnvExpr a
          enc1 <- exprRetyper
          modify $ modifyEnvExpr b
          enc2 <- exprRetyper
          let (arg1, arg2) = 
            case (enc1, enc2) of
              (Encaps j1, Encaps j2) -> (iPushToStack j1, iPushToStack j2)
          case ( correctArgType typeRepArg1 enc1
               , correctArgType typeRepArg2 enc2
               ) of
            (False, _    ) -> throwError $ 
                                FunctionArgsTypeError fName "first" pos
            (True , False) -> throwError $ 
                                FunctionArgsTypeError fName "second" pos
            (True , True ) -> do 
              case show typeRepResult of
                "[Char]"  -> return (Encaps $ retypeF2Str fName arg1 arg2)
                "Integer" -> return (Encaps $ retypeF2Int fName arg1 arg2)
                "Double"  -> return (Encaps $ retypeF2Float fName arg1 arg2)
                "Bool"    -> return (Encaps $ retypeF2Bool fName arg1 arg2)
                _ -> error "exprRetyper.CallFunc2WT.typeResult: impossible case"
        _ -> throwError $ FunctionArgsCountError fName pos
    CallFunc2WT _ _ _ -> error "exprRetyper.CallFunc2WT: impossible case"

    Slice1WT token s a -> do
      modify $ modifyEnvExpr s
      enc1 <- exprRetyper
      modify $ modifyEnvExpr a
      enc2 <- exprRetyper
      let str = fromEncapsStr enc1
      let int = fromEncapsInt enc2
      case (str, int) of
        (Just j, Just ind) -> return (Encaps $ iSlice1 j ind)
        _ -> throwError $ OpTypeError (content token) (position token)

    Slice2WT token  s a b -> do
      modify $ modifyEnvExpr s
      enc1 <- exprRetyper
      modify $ modifyEnvExpr a
      enc2 <- exprRetyper
      modify $ modifyEnvExpr b
      enc3 <- exprRetyper
      let str  = fromEncapsStr enc1
      let int1 = fromEncapsInt enc2
      let int2 = fromEncapsInt enc3
      case (str, int1, int2) of
        (Just j, Just ind1, Just ind2) -> return (Encaps $ iSlice2 j ind1 ind2)
        _ -> throwError $ OpTypeError (content token) (position token)

    InputWT -> return $ Encaps iInput

    AtomWT token ->
      case token of
        TString  _ _ value -> return (Encaps $ iValue value)
        TInteger _ _ value -> return (Encaps $ iValue value)
        TFloat   _ _ value -> return (Encaps $ iValue value)
        TBool    _ _ value -> return (Encaps $ iValue value)
        TVariable _ _ vName -> do
          case (mpVars Map.!? vName) of
            Nothing -> throwError $ VarNotDefinedError vName (position token)
            (Just typeRep) -> do
              case show typeRep of
                "[Char]"  -> return (Encaps $ retypeVarStr vName)
                "Integer" -> return (Encaps $ retypeVarInt vName)
                "Double"  -> return (Encaps $ retypeVarFloat vName)
                "Bool"    -> return (Encaps $ retypeVarBool vName)
                _ -> error "exprRetyper.AtomWT.TVariable.type: impossible case"
        _ -> error "exprRetyper.AtomWT: impossible case"

    RecWT a -> do
      modify $ modifyEnvExpr a
      enc <- exprRetyper
      case enc of
        Encaps j -> return (Encaps $iBrackets j)

-- | Function converts python code from eDSL to tagless final eDSL, 
-- and detects most of errors if they are in python code.
tfParse :: IPyScript p => String -> Except ParseException (p ())
tfParse string = case parse string of
  Left s -> throwError $ CommonParserError s
  Right statements ->
    let (result, _) = runRetyper stmtRetyper $ initRetyperEnv statements
    in result