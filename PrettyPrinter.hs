{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PrettyPrinter where

import ClassDef
import Retyper

import System.IO
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

newtype PrettyPrinter a = PrettyPrinter {toString :: String}
  deriving (Show, Semigroup)

castTS :: PrettyPrinter a -> PrettyPrinter b
castTS (PrettyPrinter s) = PrettyPrinter s

instance IStatement PrettyPrinter where
  iAssign s a = (castTS $ PrettyPrinter s) <> (PrettyPrinter " = ") <> castTS a
  iProcedure = castTS
  iPrint a = (PrettyPrinter "print(") <> castTS a <> (PrettyPrinter ")")
  iNextStmt a b = a <> (PrettyPrinter "\n") <> b

instance IExpr PrettyPrinter where
  iPlus a b = a <> (PrettyPrinter " + ") <> b
  iMinus a b = a <> (PrettyPrinter " - ") <> b
  iMult a b = a <> (PrettyPrinter " * ") <> b
  iStrPlus a b = a <> (PrettyPrinter " + ") <> b
  iInput = PrettyPrinter "input()"
  iVal   = PrettyPrinter . show
  iVarStr s = castTS $ PrettyPrinter s
  iVarInt s = castTS $ PrettyPrinter s
  iVarFloat s = castTS $ PrettyPrinter s
  iVarBool s = castTS $ PrettyPrinter s
  iHidCastIntFloat = castTS
  iBrackets a = (PrettyPrinter "( ") <> a <> (PrettyPrinter " )")

instance IPyScript PrettyPrinter

main = do
  inh <- openFile "py.py" ReadMode
  contents <- hGetContents inh
  case tfParse contents of
    Left s -> putStrLn s
    Right pyscript -> putStrLn $ toString pyscript