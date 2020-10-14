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
  iPlus a b = a <> (ToS " + ") <> b
  iMinus a b = a <> (ToS " - ") <> b
  iMult a b = a <> (ToS " * ") <> b
  iFloatDiv a b = a <> (ToS " / ") <> b
  iDiv a b = a <> (ToS " // ") <> b
  iMod a b = a <> (ToS " % ") <> b
  iVal = ToS . show
  iAssign a b = (castTS $ (ToS . show) a) <> (ToS " = ") <> castTS b
  iProcedure = castTS

prettyPrint :: Either String [Statement] -> String
prettyPrint (Left s) = s
prettyPrint (Right []) = ""
prettyPrint (Right ((Statement expr) : t)) = toString expr ++ "\n" ++ prettyPrint (Right t)

main = do
  inh <- openFile "py.py" ReadMode
  contents <- hGetContents inh
  putStrLn $ (prettyPrint . parse) contents