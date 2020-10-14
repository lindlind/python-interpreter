{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import System.IO
import Lexer
import Parser

newtype ToS a = ToS {toString :: String}
  deriving (Show, Semigroup)

castTS :: ToS a -> ToS b
castTS (ToS s) = ToS s

instance IExpr ToS where
  iAssign a b = (castTS $ (ToS . show) a) <> (ToS " = ") <> castTS b
  iProcedure = castTS
  iOr  a b = a <> (ToS " or ")  <> b
  iAnd a b = a <> (ToS " and ") <> b
  iNot a = (ToS " not ") <> a
  iBoolEq  a b = a <> (ToS " == ") <> b
  iBoolNEq a b = a <> (ToS " != ") <> b
  iEq  a b = castTS a <> (ToS " == ") <> castTS b
  iNEq a b = castTS a <> (ToS " != ") <> castTS b
  iLT  a b = castTS a <> (ToS " < ")  <> castTS b
  iGT  a b = castTS a <> (ToS " > ")  <> castTS b
  iLTE a b = castTS a <> (ToS " <= ") <> castTS b
  iGTE a b = castTS a <> (ToS " >= ") <> castTS b
  iBitOr  a b = a <> (ToS " | ") <> b
  iBitXor a b = a <> (ToS " ^ ") <> b
  iBitAnd a b = a <> (ToS " & ") <> b
  iLeftShift  a b = a <> (ToS " << ") <> b
  iRightShift a b = a <> (ToS " >> ") <> b
  iPlus a b = a <> (ToS " + ") <> b
  iMinus a b = a <> (ToS " - ") <> b
  iMult a b = a <> (ToS " * ") <> b
  iFloatDiv a b = castTS $ a <> (ToS " / ") <> b
  iDiv a b = a <> (ToS " // ") <> b
  iMod a b = a <> (ToS " % ") <> b
  iBoolVal  = ToS . show
  iIntVal   = ToS . show
  iFloatVal = ToS . show

prettyPrint :: Either String [Statement] -> String
prettyPrint (Left s) = s
prettyPrint (Right []) = ""
prettyPrint (Right ((Statement expr) : t)) = toString expr ++ "\n" ++ prettyPrint (Right t)

main = do
  inh <- openFile "py.py" ReadMode
  contents <- hGetContents inh
  putStrLn $ (prettyPrint . parse) contents