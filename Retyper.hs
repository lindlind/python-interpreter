{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Retyper where

import Lexer
import Parser
import ClassDef

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Typeable

data MyException = TypeError String

data PyType where
  PyType :: IPyType a => a -> PyType

fromPyTypeBool :: PyType -> Maybe Bool
fromPyTypeBool (PyType (enc :: t)) = do
  Refl <- eqT @t @Bool
  return enc

fromPyTypeInt :: PyType -> Maybe Integer
fromPyTypeInt (PyType (enc :: t)) = do
  Refl <- eqT @t @Integer
  return enc

fromPyTypeFloat :: PyType -> Maybe Double
fromPyTypeFloat (PyType (enc :: t)) = do
  Refl <- eqT @t @Double
  return enc

fromPyTypeStr :: PyType -> Maybe String
fromPyTypeStr (PyType (enc :: t)) = do
  Refl <- eqT @t @String
  return enc

data RetyperEnvironment = RetEnvStmt
                        { oldStmt :: StatementParse
                        , varsMap :: Map.Map String PyType
                        }
                        | RetEnvExpr
                        { oldExpr :: ExprParse
                        , varsMap :: Map.Map String PyType
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

-- type family PyTypeCast a b  :: * where
--   PyTypeCast Bool Bool       = Bool
--   PyTypeCast Integer Integer = Int
--   PyTypeCast Double Double   = Double
--   PyTypeCast Integer Double  = Double
--   PyTypeCast Double Integer  = Double
--   PyTypeCast String String   = String

-- fromEncapsPair :: (IExpr expr, IPyType a)
--                   => (expr a -> expr a -> expr a)
--                   -> Encaps expr -> Encaps expr -> Encaps expr
-- fromEncapsPair f (Encaps (e1 :: expr a)) (Encaps (e2 :: expr b)) =
--   case (eqT @a @b) of
--     (Just _) -> Encaps $ f e1 e2
--     Nothing  -> case (gcast $ iHidCastFloat e1, gcast $ iHidCastFloat e2) of
--       (Just j1, Just j2) -> Encaps $ f j1 j2

-- eqTypePair :: (IExpr expr, IPyType a, IPyType b) => expr a -> expr b -> Bool
-- eqTypePair ((e1 :: expr a)) ((e2 :: expr b)) =
--   case (eqT @a @b) of
--     (Just _) -> True
--     Nothing  -> False

modifyEnvExpr :: ExprParse -> RetyperEnvironment -> RetyperEnvironment
modifyEnvExpr newA env@RetEnvExpr{ oldExpr = a } = env { oldExpr = newA }

modifyEnvStmt :: StatementParse -> RetyperEnvironment -> RetyperEnvironment
modifyEnvStmt newA env@RetEnvStmt{ oldStmt = a } = env { oldStmt = newA }

modifyEnvMap :: (Map.Map String PyType -> Map.Map String PyType)
             -> RetyperEnvironment -> RetyperEnvironment
modifyEnvMap f env@RetEnvStmt{ varsMap = rs } = env { varsMap = f rs }

retypeVarStr :: IPyScript expr => String -> expr String
retypeVarStr name = iVariable name

retypeVarInt :: IPyScript expr => String -> expr Integer
retypeVarInt name = iVariable name

retypeVarFloat :: IPyScript expr => String -> expr Double
retypeVarFloat name = iVariable name

retypeVarBool :: IPyScript expr => String -> expr Bool
retypeVarBool name = iVariable name

exprRetyper :: IPyScript expr => RetyperMonad (Encaps expr)
exprRetyper = do
  env <- get
  let (RetEnvExpr old mp) = env
  case old of
    BinOpWT a1 token a2 -> do
      modify $ modifyEnvExpr a1
      enc1 <- exprRetyper
      modify $ modifyEnvExpr a2
      enc2 <- exprRetyper
      let bool1  = fromEncapsBool  enc1
      let bool2  = fromEncapsBool  enc2
      let int1   = fromEncapsInt   enc1
      let int2   = fromEncapsInt   enc2
      let float1 = fromEncapsFloat enc1
      let float2 = fromEncapsFloat enc2
      let str1   = fromEncapsStr   enc1
      let str2   = fromEncapsStr   enc2
      case content token of
        ("or") -> case (bool1, bool2) of
          (Just j1, Just j2) -> return (Encaps $ iOr j1 j2)
        ("and") -> case (bool1, bool2) of
          (Just j1, Just j2) -> return (Encaps $ iAnd j1 j2)

        ("==") -> case (bool1, bool2, int1, int2, float1, float2, str1, str2) of
          (Just j1, Just j2, _, _, _, _, _, _) -> return (Encaps $ iEq j1 j2)
          (_, _, Just j1, Just j2, _, _, _, _) -> return (Encaps $ iEq j1 j2)
          (_, _, Just j1, _, _, Just j2, _, _) -> return (Encaps $ iEq (iHidCastFloat j1) j2)
          (_, _, _, Just j2, Just j1, _, _, _) -> return (Encaps $ iEq j1 (iHidCastFloat j2))
          (_, _, _, _, Just j1, Just j2, _, _) -> return (Encaps $ iEq j1 j2)
          (_, _, _, _, _, _, Just j1, Just j2) -> return (Encaps $ iEq j1 j2)
        ("!=") -> case (bool1, bool2, int1, int2, float1, float2, str1, str2) of
          (Just j1, Just j2, _, _, _, _, _, _) -> return (Encaps $ iNEq j1 j2)
          (_, _, Just j1, Just j2, _, _, _, _) -> return (Encaps $ iNEq j1 j2)
          (_, _, Just j1, _, _, Just j2, _, _) -> return (Encaps $ iNEq (iHidCastFloat j1) j2)
          (_, _, _, Just j2, Just j1, _, _, _) -> return (Encaps $ iNEq j1 (iHidCastFloat j2))
          (_, _, _, _, Just j1, Just j2, _, _) -> return (Encaps $ iNEq j1 j2)
          (_, _, _, _, _, _, Just j1, Just j2) -> return (Encaps $ iNEq j1 j2)
        ("<") -> case (bool1, bool2, int1, int2, float1, float2, str1, str2) of
          (Just j1, Just j2, _, _, _, _, _, _) -> return (Encaps $ iLT j1 j2)
          (_, _, Just j1, Just j2, _, _, _, _) -> return (Encaps $ iLT j1 j2)
          (_, _, Just j1, _, _, Just j2, _, _) -> return (Encaps $ iLT (iHidCastFloat j1) j2)
          (_, _, _, Just j2, Just j1, _, _, _) -> return (Encaps $ iLT j1 (iHidCastFloat j2))
          (_, _, _, _, Just j1, Just j2, _, _) -> return (Encaps $ iLT j1 j2)
          (_, _, _, _, _, _, Just j1, Just j2) -> return (Encaps $ iLT j1 j2)
        (">") -> case (bool1, bool2, int1, int2, float1, float2, str1, str2) of
          (Just j1, Just j2, _, _, _, _, _, _) -> return (Encaps $ iGT j1 j2)
          (_, _, Just j1, Just j2, _, _, _, _) -> return (Encaps $ iGT j1 j2)
          (_, _, Just j1, _, _, Just j2, _, _) -> return (Encaps $ iGT (iHidCastFloat j1) j2)
          (_, _, _, Just j2, Just j1, _, _, _) -> return (Encaps $ iGT j1 (iHidCastFloat j2))
          (_, _, _, _, Just j1, Just j2, _, _) -> return (Encaps $ iGT j1 j2)
          (_, _, _, _, _, _, Just j1, Just j2) -> return (Encaps $ iGT j1 j2)
        ("<=") -> case (bool1, bool2, int1, int2, float1, float2, str1, str2) of
          (Just j1, Just j2, _, _, _, _, _, _) -> return (Encaps $ iLTE j1 j2)
          (_, _, Just j1, Just j2, _, _, _, _) -> return (Encaps $ iLTE j1 j2)
          (_, _, Just j1, _, _, Just j2, _, _) -> return (Encaps $ iLTE (iHidCastFloat j1) j2)
          (_, _, _, Just j2, Just j1, _, _, _) -> return (Encaps $ iLTE j1 (iHidCastFloat j2))
          (_, _, _, _, Just j1, Just j2, _, _) -> return (Encaps $ iLTE j1 j2)
          (_, _, _, _, _, _, Just j1, Just j2) -> return (Encaps $ iLTE j1 j2)
        (">=") -> case (bool1, bool2, int1, int2, float1, float2, str1, str2) of
          (Just j1, Just j2, _, _, _, _, _, _) -> return (Encaps $ iGTE j1 j2)
          (_, _, Just j1, Just j2, _, _, _, _) -> return (Encaps $ iGTE j1 j2)
          (_, _, Just j1, _, _, Just j2, _, _) -> return (Encaps $ iGTE (iHidCastFloat j1) j2)
          (_, _, _, Just j2, Just j1, _, _, _) -> return (Encaps $ iGTE j1 (iHidCastFloat j2))
          (_, _, _, _, Just j1, Just j2, _, _) -> return (Encaps $ iGTE j1 j2)
          (_, _, _, _, _, _, Just j1, Just j2) -> return (Encaps $ iGTE j1 j2)

        ("|") -> case (int1, int2) of
          (Just j1, Just j2) -> return (Encaps $ iBitOr j1 j2)
        ("^") -> case (int1, int2) of
          (Just j1, Just j2) -> return (Encaps $ iBitXor j1 j2)
        ("&") -> case (int1, int2) of
          (Just j1, Just j2) -> return (Encaps $ iBitAnd j1 j2)
        ("<<") -> case (int1, int2) of
          (Just j1, Just j2) -> return (Encaps $ iLeftShift j1 j2)
        (">>") -> case (int1, int2) of
          (Just j1, Just j2) -> return (Encaps $ iRightShift j1 j2)

        ("+") -> case (int1, int2, float1, float2, str1, str2) of
          (Just j1, Just j2, _, _, _, _) -> return (Encaps $ iPlus j1 j2)
          (Just j1, _, _, Just j2, _, _) -> return (Encaps $ iPlus (iHidCastFloat j1) j2)
          (_, Just j2, Just j1, _, _, _) -> return (Encaps $ iPlus j1 (iHidCastFloat j2))
          (_, _, Just j1, Just j2, _, _) -> return (Encaps $ iPlus j1 j2)
          (_, _, _, _, Just j1, Just j2) -> return (Encaps $ iStrPlus j1 j2)
        ("-") -> case (int1, int2, float1, float2) of
          (Just j1, Just j2, _, _) -> return (Encaps $ iMinus j1 j2)
          (Just j1, _, _, Just j2) -> return (Encaps $ iMinus (iHidCastFloat j1) j2)
          (_, Just j2, Just j1, _) -> return (Encaps $ iMinus j1 (iHidCastFloat j2))
          (_, _, Just j1, Just j2) -> return (Encaps $ iMinus j1 j2)
        ("*") -> case (int1, int2, float1, float2) of
          (Just j1, Just j2, _, _) -> return (Encaps $ iMult j1 j2)
          (Just j1, _, _, Just j2) -> return (Encaps $ iMult (iHidCastFloat j1) j2)
          (_, Just j2, Just j1, _) -> return (Encaps $ iMult j1 (iHidCastFloat j2))
          (_, _, Just j1, Just j2) -> return (Encaps $ iMult j1 j2)
        ("/") -> case (int1, int2, float1, float2) of
          (Just j1, Just j2, _, _) -> return (Encaps $ iFloatDiv (iHidCastFloat j1)
                                                                 (iHidCastFloat j2)
                                                                 )
          (Just j1, _, _, Just j2) -> return (Encaps $ iFloatDiv (iHidCastFloat j1) j2)
          (_, Just j2, Just j1, _) -> return (Encaps $ iFloatDiv j1 (iHidCastFloat j2))
          (_, _, Just j1, Just j2) -> return (Encaps $ iFloatDiv j1 j2)
        ("//") -> case (int1, int2) of
          (Just j1, Just j2) -> return (Encaps $ iDiv j1 j2)
        ("%") -> case (int1, int2) of
          (Just j1, Just j2) -> return (Encaps $ iMod j1 j2)
        ("**") -> case (int1, int2, float1, float2) of
          (Just j1, Just j2, _, _) -> return (Encaps $ iPow (iHidCastFloat j1)
                                                            (iHidCastFloat j2)
                                                            )
          (Just j1, _, _, Just j2) -> return (Encaps $ iPow (iHidCastFloat j1) j2)
          (_, Just j2, Just j1, _) -> return (Encaps $ iPow j1 (iHidCastFloat j2))
          (_, _, Just j1, Just j2) -> return (Encaps $ iPow j1 j2)

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
      let str   = fromEncapsStr   enc
      let int   = fromEncapsInt   enc
      let float = fromEncapsFloat enc
      let bool  = fromEncapsBool  enc
      case content token of
        ("str") -> case (str, int, float, bool) of
          (Just j, _, _, _) -> return (Encaps $ iCastStr j)
          (_, Just j, _, _) -> return (Encaps $ iCastStr j)
          (_, _, Just j, _) -> return (Encaps $ iCastStr j)
          (_, _, _, Just j) -> return (Encaps $ iCastStr j)
        ("int") -> case (str, int, float, bool) of
          (Just j, _, _, _) -> return (Encaps $ iCastInt j)
          (_, Just j, _, _) -> return (Encaps $ iCastInt j)
          (_, _, Just j, _) -> return (Encaps $ iCastInt j)
          (_, _, _, Just j) -> return (Encaps $ iCastInt j)
        ("float") -> case (str, int, float, bool) of
          (Just j, _, _, _) -> return (Encaps $ iCastFloat j)
          (_, Just j, _, _) -> return (Encaps $ iCastFloat j)
          (_, _, Just j, _) -> return (Encaps $ iCastFloat j)
          (_, _, _, Just j) -> return (Encaps $ iCastFloat j)
        ("bool") -> case (str, int, float, bool) of
          (Just j, _, _, _) -> return (Encaps $ iCastBool j)
          (_, Just j, _, _) -> return (Encaps $ iCastBool j)
          (_, _, Just j, _) -> return (Encaps $ iCastBool j)
          (_, _, _, Just j) -> return (Encaps $ iCastBool j)
    InputWT -> return $ Encaps iInput

    AtomWT token ->
      case token of
        TString  _ _ value -> return (Encaps $ iValue value)
        TInteger _ _ value -> return (Encaps $ iValue value)
        TFloat   _ _ value -> return (Encaps $ iValue value)
        TBool    _ _ value -> return (Encaps $ iValue value)
        TVariable _ _ name -> do
          let enc = mp Map.! name
          let bool = fromPyTypeBool enc
          let int = fromPyTypeInt enc
          let float = fromPyTypeFloat enc
          let str = fromPyTypeStr enc
          case (str, int, float, bool) of
            (Just _, _, _, _) -> return (Encaps $ retypeVarStr name)
            (_, Just _, _, _) -> return (Encaps $ retypeVarInt name)
            (_, _, Just _, _) -> return (Encaps $ retypeVarFloat name)
            (_, _, _, Just _) -> return (Encaps $ retypeVarBool name)

    RecWT a -> do
      modify $ modifyEnvExpr a
      enc <- exprRetyper
      case enc of
        Encaps j -> return (Encaps $iBrackets j)

stmtRetyper :: IPyScript stmt => RetyperMonad (stmt ())
stmtRetyper = do
  env <- get
  let (RetEnvStmt old mp) = env
  case old of
    IfWT e a -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mp
      let (Just cond) = fromEncapsBool enc
      modify $ modifyEnvStmt a
      thn <- stmtRetyper
      put env
      return (iIf cond thn)

    IfElseWT e a b -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mp
      let (Just cond) = fromEncapsBool enc
      modify $ modifyEnvStmt a
      thn <- stmtRetyper
      put env
      modify $ modifyEnvStmt b
      els <- stmtRetyper
      put env
      return (iIfElse cond thn els)

    WhileWT e a -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mp
      let (Just cond) = fromEncapsBool enc
      modify $ modifyEnvStmt a
      thn <- stmtRetyper
      put env
      return (iWhile cond thn)

    AssignWT token@(TVariable _ _ name) e -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mp
      let bool = fromEncapsBool enc
      let int = fromEncapsInt enc
      let float = fromEncapsFloat enc
      let str = fromEncapsStr enc
      case (bool, int, float, str) of
        (Just j, _, _, _) -> do
          modify $ modifyEnvMap (Map.insert name $ PyType False)
          return (iAssign name j)
        (_, Just j, _, _) -> do
          modify $ modifyEnvMap (Map.insert name $ PyType (0 :: Integer))
          return (iAssign name j)
        (_, _, Just j, _) -> do
          modify $ modifyEnvMap (Map.insert name $ PyType (0 :: Double))
          return (iAssign name j)
        (_, _, _, Just j) -> do
          modify $ modifyEnvMap (Map.insert name $ PyType "")
          return (iAssign name j)

    ProcedureWT e -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mp
      case enc of
        Encaps j -> return (iProcedure j)

    PrintWT e -> do
      let (enc, _) = runState exprRetyper $ RetEnvExpr e mp
      case enc of
        Encaps j -> return (iPrint j)

    BreakWT -> return iBreak

    ContinueWT -> return iContinue

    StmtPrsSeq a b -> do
      modify $ modifyEnvStmt a
      r1 <- stmtRetyper
      modify $ modifyEnvStmt b
      r2 <- stmtRetyper
      return (iNextStmt r1 r2)

tfParse :: IPyScript p => String -> Either String (p ())
tfParse string = case parse string of
  Left s -> Left s
  Right statements ->
    let (pyscript, env) = runState stmtRetyper $
           RetEnvStmt {oldStmt = statements, varsMap = Map.empty}
    in Right pyscript