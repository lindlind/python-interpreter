{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE InstanceSigs #-}

module Retyper where

import Lexer
import Parser
import ClassDef

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Applicative
import qualified Data.Map.Strict as Map
import Data.Typeable

data RetyperEnvironment = RetEnvStmt { oldStmt :: StatementParse
                                     , varsMap :: Map.Map String TypeRep
                                     , funcsMap :: Map.Map String TypeRep
                                     }
                                     | RetEnvExpr
                                     { oldExpr :: ExprParse
                                     , varsMap :: Map.Map String TypeRep
                                     , funcsMap :: Map.Map String TypeRep
                                     }

initRetyperEnv :: StatementParse -> RetyperEnvironment
initRetyperEnv statements = RetEnvStmt { oldStmt = statements
                                       , varsMap = Map.empty
                                       , funcsMap = Map.empty
                                       }

type RetyperMonad a = State RetyperEnvironment a

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
modifyEnvExpr newA env@RetEnvExpr{ oldExpr = a } = env { oldExpr = newA }

modifyEnvStmt :: StatementParse -> RetyperEnvironment -> RetyperEnvironment
modifyEnvStmt newA env@RetEnvStmt{ oldStmt = a } = env { oldStmt = newA }

modifyEnvMapVars :: (Map.Map String TypeRep -> Map.Map String TypeRep)
                    -> RetyperEnvironment -> RetyperEnvironment
modifyEnvMapVars f env@RetEnvStmt{ varsMap = mp } = env { varsMap = f mp }

modifyEnvMapFuncs :: (Map.Map String TypeRep -> Map.Map String TypeRep)
                     -> RetyperEnvironment -> RetyperEnvironment
modifyEnvMapFuncs f env@RetEnvStmt{ funcsMap = mp } = env { funcsMap = f mp }

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

tryBinOpStr :: (IPyScript expr, IPyType t) => (expr String -> expr String -> expr t)
               -> Encaps expr -> Encaps expr -> Maybe (Encaps expr)
tryBinOpStr binOp enc1 enc2 = do
  j1 <- fromEncapsStr enc1
  j2 <- fromEncapsStr enc2
  return $ Encaps $ binOp j1 j2

tryBinOpInt :: (IPyScript expr, IPyType t) => (expr Integer -> expr Integer -> expr t)
               -> Encaps expr -> Encaps expr -> Maybe (Encaps expr)
tryBinOpInt binOp enc1 enc2 = do
  j1 <- fromEncapsInt enc1
  j2 <- fromEncapsInt enc2
  return $ Encaps $ binOp j1 j2

tryBinOpFloat :: (IPyScript expr, IPyType t) => (expr Double -> expr Double -> expr t)
                 -> Encaps expr -> Encaps expr -> Maybe (Encaps expr)
tryBinOpFloat binOp enc1 enc2 = do
  j1 <- ( fromEncapsInt enc1 >>= \int -> return (iHidCastFloat int) ) <|> fromEncapsFloat enc1
  j2 <- ( fromEncapsInt enc2 >>= \int -> return (iHidCastFloat int) ) <|> fromEncapsFloat enc2
  return $ Encaps $ binOp j1 j2

tryBinOpBool :: (IPyScript expr, IPyType t) => (expr Bool -> expr Bool -> expr t)
                 -> Encaps expr -> Encaps expr -> Maybe (Encaps expr)
tryBinOpBool binOp enc1 enc2 = do
  j1 <- fromEncapsBool enc1
  j2 <- fromEncapsBool enc2
  return $ Encaps $ binOp j1 j2

stmtRetyper :: IPyScript stmt => RetyperMonad (stmt ())
stmtRetyper = do
  env <- get
  let (RetEnvStmt old mpVars mpFuncs) = env
  case old of
    IfWT e a -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mpVars mpFuncs
      let (Just cond) = fromEncapsBool enc
      modify $ modifyEnvStmt a
      thn <- stmtRetyper
      return (iIf cond thn)

    IfElseWT e a b -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mpVars mpFuncs
      let (Just cond) = fromEncapsBool enc
      modify $ modifyEnvStmt a
      thn <- stmtRetyper
      modify $ modifyEnvStmt b
      els <- stmtRetyper
      return (iIfElse cond thn els)

    WhileWT e a -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mpVars mpFuncs
      let (Just cond) = fromEncapsBool enc
      modify $ modifyEnvStmt a
      thn <- stmtRetyper
      return (iWhile cond thn)

    BreakWT -> return iBreak

    ContinueWT -> return iContinue

    DefFunc0WT (TVariable _ _ name) 
               (TType _ _ resultType) a -> do
      modify $ modifyEnvMapFuncs (Map.insert name $ typeStrToRep resultType)
      modify $ modifyEnvStmt a
      body <- stmtRetyper
      put env
      modify $ modifyEnvMapFuncs (Map.insert name $ typeStrToRep resultType)
      return (iDefFunc0 name body)

    DefFunc1WT (TVariable _ _ name)
               (TVariable _ _ argName) (TType _ _ argType)
               (TType _ _ resultType) a -> do
      modify $ modifyEnvMapVars  (Map.insert argName $ typeStrToRep argType)
      modify $ modifyEnvMapFuncs (Map.insert name $ typeStrToRep resultType)
      modify $ modifyEnvStmt a
      body <- stmtRetyper
      put env
      modify $ modifyEnvMapFuncs (Map.insert name $ typeStrToRep resultType)
      return (iDefFunc1 name argName body)

    DefFunc2WT (TVariable _ _ name)
               (TVariable _ _ arg1Name) (TType _ _ arg1Type)
               (TVariable _ _ arg2Name) (TType _ _ arg2Type)
               (TType _ _ resultType) a -> do
      modify $ modifyEnvMapVars  (Map.insert arg1Name $ typeStrToRep arg1Type)      
      modify $ modifyEnvMapVars  (Map.insert arg2Name $ typeStrToRep arg2Type)
      modify $ modifyEnvMapFuncs (Map.insert name $ typeStrToRep resultType)
      modify $ modifyEnvStmt a
      body <- stmtRetyper
      put env
      modify $ modifyEnvMapFuncs (Map.insert name $ typeStrToRep resultType)
      return (iDefFunc2 name arg1Name arg2Name body)

    ReturnWT e -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mpVars mpFuncs
      case enc of
        (Encaps j) -> return (iReturn j)

    AssignWT token@(TVariable _ _ name) e -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mpVars mpFuncs
      let bool = fromEncapsBool enc
      let int = fromEncapsInt enc
      let float = fromEncapsFloat enc
      let str = fromEncapsStr enc
      case (str, int, float, bool) of
        (Just j, _, _, _) -> do
          modify $ modifyEnvMapVars (Map.insert name $ typeOf (def :: String))
          return (iAssign name j)        
        (_, Just j, _, _) -> do
          modify $ modifyEnvMapVars (Map.insert name $ typeOf (def :: Integer))
          return (iAssign name j)
        (_, _, Just j, _) -> do
          modify $ modifyEnvMapVars (Map.insert name $ typeOf (def :: Double))
          return (iAssign name j)
        (_, _, _, Just j) -> do
          modify $ modifyEnvMapVars (Map.insert name $ typeOf (def :: Bool))
          return (iAssign name j)

    ProcedureWT e -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mpVars mpFuncs
      case enc of
        Encaps j -> return (iProcedure j)

    PrintWT e -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mpVars mpFuncs
      case enc of
        Encaps j -> return (iPrint j)

    StmtPrsSeq a b -> do
      modify $ modifyEnvStmt a
      r1 <- stmtRetyper
      modify $ modifyEnvStmt b
      r2 <- stmtRetyper
      return (iNextStmt r1 r2)


exprRetyper :: IPyScript expr => RetyperMonad (Encaps expr)
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
        ("and") -> do
          let res = tryBinOpBool iAnd enc1 enc2
          case res of
            Just j -> return j

        ("==") -> do
          let res = tryBinOpStr   iEq enc1 enc2 
                <|> tryBinOpInt   iEq enc1 enc2 
                <|> tryBinOpFloat iEq enc1 enc2 
                <|> tryBinOpBool  iEq enc1 enc2
          case res of
            Just j -> return j
        ("!=") -> do
          let res = tryBinOpStr   iNEq enc1 enc2 
                <|> tryBinOpInt   iNEq enc1 enc2 
                <|> tryBinOpFloat iNEq enc1 enc2 
                <|> tryBinOpBool  iNEq enc1 enc2
          case res of
            Just j -> return j
        ("<") -> do
          let res = tryBinOpStr   iLT enc1 enc2 
                <|> tryBinOpInt   iLT enc1 enc2 
                <|> tryBinOpFloat iLT enc1 enc2 
                <|> tryBinOpBool  iLT enc1 enc2
          case res of
            Just j -> return j
        (">") -> do
          let res = tryBinOpStr   iGT enc1 enc2 
                <|> tryBinOpInt   iGT enc1 enc2 
                <|> tryBinOpFloat iGT enc1 enc2 
                <|> tryBinOpBool  iGT enc1 enc2
          case res of
            Just j -> return j
        ("<=") -> do
          let res = tryBinOpStr   iLTE enc1 enc2 
                <|> tryBinOpInt   iLTE enc1 enc2 
                <|> tryBinOpFloat iLTE enc1 enc2 
                <|> tryBinOpBool  iLTE enc1 enc2
          case res of
            Just j -> return j
        (">=") -> do
          let res = tryBinOpStr   iGTE enc1 enc2 
                <|> tryBinOpInt   iGTE enc1 enc2 
                <|> tryBinOpFloat iGTE enc1 enc2 
                <|> tryBinOpBool  iGTE enc1 enc2
          case res of
            Just j -> return j

        ("|") -> do
          let res = tryBinOpInt iBitOr enc1 enc2
          case res of
            Just j -> return j
        ("^") -> do
          let res = tryBinOpInt iBitXor enc1 enc2
          case res of
            Just j -> return j
        ("&") -> do
          let res = tryBinOpInt iBitAnd enc1 enc2
          case res of
            Just j -> return j
        ("<<") -> do
          let res = tryBinOpInt iLeftShift enc1 enc2
          case res of
            Just j -> return j
        (">>") -> do
          let res = tryBinOpInt iRightShift enc1 enc2
          case res of
            Just j -> return j

        ("+") -> do
          let res = tryBinOpInt    iPlus enc1 enc2 
                <|> tryBinOpFloat  iPlus enc1 enc2 
                <|> tryBinOpStr iStrPlus enc1 enc2
          case res of
            Just j -> return j
        ("-") -> do
          let res = tryBinOpInt iMinus enc1 enc2 <|> tryBinOpFloat iMinus enc1 enc2
          case res of
            Just j -> return j
        ("*") -> do
          let res = tryBinOpInt iMinus enc1 enc2 <|> tryBinOpFloat iMult enc1 enc2
          case res of
            Just j -> return j
        ("/") -> do
          let res = tryBinOpFloat iFloatDiv enc1 enc2
          case res of
            Just j -> return j
        ("//") -> do
          let res = tryBinOpInt iDiv enc1 enc2
          case res of
            Just j -> return j
        ("%") -> do
          let res = tryBinOpInt iMod enc1 enc2
          case res of
            Just j -> return j
        ("**") -> do
          let res = tryBinOpFloat iPow enc1 enc2
          case res of
            Just j -> return j

    UnOpWT token a -> do
      modify $ modifyEnvExpr a
      enc <- exprRetyper
      let bool  = fromEncapsBool  enc
      let int   = fromEncapsInt   enc
      let float = fromEncapsFloat enc
      let str   = fromEncapsStr   enc
      case content token of
        ("not") -> case bool of
          (Just j) -> return (Encaps $ iNot j)
        ("+") -> case (int, float) of
          (Just j, _) -> return (Encaps $ iUnarPlus j)
          (_, Just j) -> return (Encaps $ iUnarPlus j)
        ("-") -> case (int, float) of
          (Just j, _) -> return (Encaps $ iUnarMinus j)
          (_, Just j) -> return (Encaps $ iUnarMinus j)

    CastWT token a -> do
      modify $ modifyEnvExpr a
      enc <- exprRetyper
      case content token of
        ("str") -> do
          let res = tryUnOpStr   iCastStr enc <|> tryUnOpInt  iCastStr enc
                <|> tryUnOpFloat iCastStr enc <|> tryUnOpBool iCastStr enc
          case res of
            Just j -> return j
        ("int") -> do
          let res = tryUnOpStr   iCastInt enc <|> tryUnOpInt  iCastInt enc
                <|> tryUnOpFloat iCastInt enc <|> tryUnOpBool iCastInt enc
          case res of
            Just j -> return j
        ("float") -> do
          let res = tryUnOpStr   iCastFloat enc <|> tryUnOpInt  iCastFloat enc
                <|> tryUnOpFloat iCastFloat enc <|> tryUnOpBool iCastFloat enc
          case res of
            Just j -> return j
        ("bool") -> do
          let res = tryUnOpStr   iCastBool enc <|> tryUnOpInt  iCastBool enc
                <|> tryUnOpFloat iCastBool enc <|> tryUnOpBool iCastBool enc
          case res of
            Just j -> return j

    CallFunc0WT (TVariable _ _ name) -> do
      let typeRepResult = mpFuncs Map.! name
      case show typeRepResult of
        "[Char]"  -> return (Encaps $ retypeF0Str name)
        "Integer" -> return (Encaps $ retypeF0Int name)
        "Double"  -> return (Encaps $ retypeF0Float name)
        "Bool"    -> return (Encaps $ retypeF0Bool name)

    CallFunc1WT (TVariable _ _ name) a -> do
      let typeRepResult = mpFuncs Map.! name
      modify $ modifyEnvExpr a
      enc <- exprRetyper
      let arg = case enc of 
                  Encaps j -> iPushToStack j
      let typeRep = mpFuncs Map.! name
      case show typeRep of
        "[Char]"  -> return (Encaps $ retypeF1Str name arg)
        "Integer" -> return (Encaps $ retypeF1Int name arg)
        "Double"  -> return (Encaps $ retypeF1Float name arg)
        "Bool"    -> return (Encaps $ retypeF1Bool name arg)

    CallFunc2WT (TVariable _ _ name) a b -> do
      let typeRepResult = mpFuncs Map.! name
      modify $ modifyEnvExpr a
      enc1 <- exprRetyper
      modify $ modifyEnvExpr b
      enc2 <- exprRetyper
      let (arg1, arg2) = case (enc1, enc2) of
                          (Encaps j1, Encaps j2) -> (iPushToStack j1, iPushToStack j2)
      let typeRep = mpFuncs Map.! name
      case show typeRep of
        "[Char]"  -> return (Encaps $ retypeF2Str name arg1 arg2)
        "Integer" -> return (Encaps $ retypeF2Int name arg1 arg2)
        "Double"  -> return (Encaps $ retypeF2Float name arg1 arg2)
        "Bool"    -> return (Encaps $ retypeF2Bool name arg1 arg2)

    Slice1WT s a -> do
      modify $ modifyEnvExpr s
      enc1 <- exprRetyper
      modify $ modifyEnvExpr a
      enc2 <- exprRetyper
      let str = fromEncapsStr enc1
      let int = fromEncapsInt enc2
      case (str, int) of
        (Just j, Just ind) -> return (Encaps $ iSlice1 j ind)

    Slice2WT s a b -> do
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

    InputWT -> return $ Encaps iInput

    AtomWT token ->
      case token of
        TString  _ _ value -> return (Encaps $ iValue value)
        TInteger _ _ value -> return (Encaps $ iValue value)
        TFloat   _ _ value -> return (Encaps $ iValue value)
        TBool    _ _ value -> return (Encaps $ iValue value)
        TVariable _ _ name -> do
          let typeRep = mpVars Map.! name
          case show typeRep of
            "[Char]"  -> return (Encaps $ retypeVarStr name)
            "Integer" -> return (Encaps $ retypeVarInt name)
            "Double"  -> return (Encaps $ retypeVarFloat name)
            "Bool"    -> return (Encaps $ retypeVarBool name)

    RecWT a -> do
      modify $ modifyEnvExpr a
      enc <- exprRetyper
      case enc of
        Encaps j -> return (Encaps $iBrackets j)

tfParse :: IPyScript p => String -> Either String (p ())
tfParse string = case parse string of
  Left s -> Left s
  Right statements ->
    let (pyscript, env) = runState stmtRetyper $ initRetyperEnv statements
    in Right pyscript