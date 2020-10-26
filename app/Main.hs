module Main where

import Interpreter
  ( parseAndInterpret
  )
import PrettyPrinter
  ( parseAndPrettyPrint
  )

import System.Directory 
  ( doesFileExist
  )
import System.IO
  ( IOMode (ReadMode)
  , openFile
  , hGetContents
  )

getFilename :: IO (String)
getFilename = do
  filename <- getLine
  let isPy = ".py" == drop (length filename - 3) filename
  if not isPy
  then do
    putStrLn "File should be *.py"
    putStrLn "print another filename:"
    getFilename
  else do
    isFile <- doesFileExist $ "pysrc\\" ++ filename
    if isFile
    then return $ "pysrc\\" ++ filename
    else do
      putStrLn "File not found."
      putStrLn $ "Put file in .\\pysrc\\ "
                  ++ "and print only name of file without directory."
      putStrLn "print another filename:"
      getFilename

getMode :: IO (Bool)
getMode = do
  query <- getLine
  case query of
    "i" -> return True
    "interpret" -> return True
    "p" -> return False
    "print" -> return False
    "pretty-print" -> return False
    _ -> do
      putStrLn "Wrong mode."
      putStrLn "Print 'i' or 'interpret' to interpret python code."
      putStrLn $ "Print 'p' or 'print' or 'pretty-print' "
                  ++ "to print python code in pretty form."
      putStrLn "print correct mode:"
      getMode

main :: IO ()
main = do
  putStrLn "Print mode:"
  mode <- getMode
  putStrLn "Print filename:"
  filename <- getFilename
  inh <- openFile filename ReadMode
  pyscript <- hGetContents inh
  case mode of
    False -> putStrLn $ parseAndPrettyPrint pyscript
    True -> parseAndInterpret pyscript