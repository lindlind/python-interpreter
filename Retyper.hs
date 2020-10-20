{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstrainedClassMethods#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Retyper where

import Lexer
import Parser

import System.IO
import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Typeable

data MyException = TypeError String

class (Typeable t, Ord t, Show t) => IPyType t
instance IPyType Bool
instance IPyType Double
instance IPyType Int
instance IPyType String

class IPyType t => IPyNumType t
instance IPyNumType Double
instance IPyNumType Int

data PyType where
  PyType :: IPyType a => a -> PyType

data RetyperEnvironment = RetEnvStmt
                        {
                           oldStmts :: StatementParse
                        ,  varsMap :: Map.Map String PyType
                        }
                        | RetEnvExpr
                        {
                           oldExprs :: ExprParse
                        ,  varsMap :: Map.Map String PyType
                        }

type RetyperMonad a = State RetyperEnvironment a

class IStatement stmt where
  iAssign    :: IExpr stmt => String -> stmt t -> stmt ()
  iProcedure :: IExpr stmt => stmt t -> stmt ()
  iNextStmt :: stmt () -> stmt () -> stmt ()

class IExpr expr where
  iPlus :: expr Int -> expr Int -> expr Int
  iIntVal :: Int -> expr Int
  iBrackets :: expr t -> expr t

class (IStatement p, IExpr p) => IPyScript p 

data Encaps stmt where
  Encaps :: (IExpr expr, IPyType t) => expr t -> Encaps expr

fromEncapsInt :: Encaps expr -> Maybe (expr Int)
fromEncapsInt (Encaps (enc :: expr t)) = do
  Refl <- eqT @t @Int
  return enc

exprRetyper :: IPyScript expr => RetyperMonad (Encaps expr)
exprRetyper = StateT $ \env ->
  case env of 
    RetEnvExpr old mp ->
      case old of
        BinOpWT a1 token a2 ->
          let 
            (enc1, env1) = runState exprRetyper $ RetEnvExpr a1 mp
            (enc2, env2) = runState exprRetyper $ RetEnvExpr a2 mp
            r1 = fromEncapsInt enc1
            r2 = fromEncapsInt enc2
          in
            case (r1, r2, content token) of
              (Just j1, Just j2, "+") -> return (Encaps $ iPlus j1 j2, env)
        AtomWT token ->
          case token of
            TInt _ _ value -> return (Encaps $ iIntVal value, env)

stmtRetyper :: IPyScript stmt => RetyperMonad (stmt ())
stmtRetyper = StateT $ \env ->
  case env of
    RetEnvStmt old mp ->
      case old of
        (ProcedureWT a) ->
          let
            (enc, env) = runState exprRetyper $ RetEnvExpr a mp
            r = fromEncapsInt
          in
            case enc of
              Encaps j -> return (iProcedure j, env)

newtype ToS a = ToS {toString :: String}
  deriving (Show, Semigroup)

castTS :: ToS a -> ToS b
castTS (ToS s) = ToS s

instance IStatement ToS where
  iAssign a b = (castTS $ ToS a) <> (ToS " = ") <> castTS b
  iProcedure = castTS

instance IExpr ToS where
  iPlus a b = a <> (ToS " + ") <> b
  iIntVal   = ToS . show
  iBrackets a = (ToS "( ") <> a <> (ToS " )")

instance IPyScript ToS

main = do
  inh <- openFile "py.py" ReadMode
  contents <- hGetContents inh
  case parse contents of
    Left s -> putStrLn s
    Right statements ->
      let (pyscript, env) = runState stmtRetyper $ RetEnvStmt {oldStmts = statements, varsMap = Map.empty}
      in do
        putStrLn $ toString pyscript