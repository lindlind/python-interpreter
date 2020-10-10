module Main where

import System.IO
import Lexer
import Parser

main = do
  inh <- openFile "py.py" ReadMode
  contents <- hGetContents inh
  return $ parse contents