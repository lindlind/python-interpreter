{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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

exprRetyper :: IPyScript expr => RetyperMonad (Encaps expr)
exprRetyper = StateT $ \env@(RetEnvExpr old mp) ->
  case old of
    BinOpWT a1 token a2 ->
      let 
        (enc1, _) = runState exprRetyper $ RetEnvExpr a1 mp
        (enc2, _) = runState exprRetyper $ RetEnvExpr a2 mp
        int1 = fromEncapsInt enc1
        int2 = fromEncapsInt enc2
        float1 = fromEncapsFloat enc1
        float2 = fromEncapsFloat enc2
        str1 = fromEncapsStr enc1
        str2 = fromEncapsStr enc2
      in case content token of
        ("+") -> case (int1, int2, float1, float2, str1, str2) of
          (Just j1, Just j2, _, _, _, _) -> return (Encaps $ iPlus j1 j2, env)
          (Just j1, _, _, Just j2, _, _) -> return (Encaps $ iPlus (iHidCastIntFloat j1) j2, env)
          (_, Just j2, Just j1, _, _, _) -> return (Encaps $ iPlus j1 (iHidCastIntFloat j2), env)
          (_, _, Just j1, Just j2, _, _) -> return (Encaps $ iPlus j1 j2, env)
          (_, _, _, _, Just j1, Just j2) -> return (Encaps $ iStrPlus j1 j2, env)
        ("-") -> case (int1, int2, float1, float2) of
          (Just j1, Just j2, _, _) -> return (Encaps $ iMinus j1 j2, env)
          (Just j1, _, _, Just j2) -> return (Encaps $ iMinus (iHidCastIntFloat j1) j2, env)
          (_, Just j2, Just j1, _) -> return (Encaps $ iMinus j1 (iHidCastIntFloat j2), env)
          (_, _, Just j1, Just j2) -> return (Encaps $ iMinus j1 j2, env)
        ("*") -> case (int1, int2, float1, float2) of
          (Just j1, Just j2, _, _) -> return (Encaps $ iMult j1 j2, env)
          (Just j1, _, _, Just j2) -> return (Encaps $ iMult (iHidCastIntFloat j1) j2, env)
          (_, Just j2, Just j1, _) -> return (Encaps $ iMult j1 (iHidCastIntFloat j2), env)
          (_, _, Just j1, Just j2) -> return (Encaps $ iMult j1 j2, env)

    InputWT -> return (Encaps iInput, env)

    AtomWT token ->
      case token of
        TString  _ _ value -> return (Encaps $ iVal value, env)
        TInteger _ _ value -> return (Encaps $ iVal value, env)
        TFloat   _ _ value -> return (Encaps $ iVal value, env)
        TBool    _ _ value -> return (Encaps $ iVal value, env)
        TVariable _ _ name ->
          case mp Map.!? name of
            Just enc ->
              let
                int = fromPyTypeInt enc
                float = fromPyTypeFloat enc
                str = fromPyTypeStr enc
              in case (int, float, str) of
                (Just _, _, _) -> return (Encaps $ iVarInt name, env)
                (_, Just _, _) -> return (Encaps $ iVarFloat name, env)
                (_, _, Just _) -> return (Encaps $ iVarStr name, env)





stmtRetyper :: IPyScript stmt => RetyperMonad (stmt ())
stmtRetyper = StateT $ \env@(RetEnvStmt old mp) ->
  case old of
    AssignWT token@(TVariable _ _ name) a ->
      let
        (enc, _) = runState exprRetyper $ RetEnvExpr a mp
        int = fromEncapsInt enc
        float = fromEncapsFloat enc
        str = fromEncapsStr enc
      in
        case (int, float, str) of
          (Just j, _, _) -> return (iAssign name j, RetEnvStmt old $ Map.insert name (PyType (0 :: Integer)) mp)
          (_, Just j, _) -> return (iAssign name j, RetEnvStmt old $ Map.insert name (PyType (0 :: Double)) mp)
          (_, _, Just j) -> return (iAssign name j, RetEnvStmt old $ Map.insert name (PyType ("" :: String)) mp)

    ProcedureWT a ->
      let 
        (enc, _) = runState exprRetyper $ RetEnvExpr a mp
      in
        case enc of
          Encaps j -> return (iProcedure j, env)

    PrintWT a -> 
      let
        (enc, _) = runState exprRetyper $ RetEnvExpr a mp
      in 
        case enc of
          Encaps j -> return (iPrint j, env)

    StmtPrsSeq a b ->
      let
        (r1, RetEnvStmt _ mp1) = runState stmtRetyper $ RetEnvStmt a mp
        (r2, env2) = runState stmtRetyper $ RetEnvStmt b mp1
      in return (iNextStmt r1 r2, env2)

tfParse :: IPyScript p => String -> Either String (p ())
tfParse string = case parse string of
  Left s -> Left s
  Right statements ->
    let (pyscript, env) = runState stmtRetyper $ 
                            RetEnvStmt {oldStmt = statements, varsMap = Map.empty}
    in Right pyscript 