{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PrettyPrinter where

import ClassDef
import Retyper
import Parser

import System.IO
import Control.Monad.State.Strict

data PrinterEnviroment  = PrintEnv { asString :: String
                                   , indent :: Int
                                   }

type PrettyPrinter = State PrinterEnviroment

concatEnv :: String -> PrinterEnviroment -> PrinterEnviroment
concatEnv s2 env@PrintEnv{ asString = s1 } = env { asString = s1 ++ s2 }

incIndentEnv :: PrinterEnviroment -> PrinterEnviroment
incIndentEnv env@PrintEnv{ indent = x } = env { indent = x + 1 }

decIndentEnv :: PrinterEnviroment -> PrinterEnviroment
decIndentEnv env@PrintEnv{ indent = x } = env { indent = x - 1 }

concatPrinter :: String -> PrettyPrinter ()
concatPrinter s = modify $ concatEnv s

concatPrinterSafeType :: String -> a -> PrettyPrinter a
concatPrinterSafeType s = \a -> do
  modify $ concatEnv s
  return a

incIndentPrinter :: PrettyPrinter ()
incIndentPrinter = modify incIndentEnv

decIndentPrinter :: PrettyPrinter ()
decIndentPrinter = modify decIndentEnv

newlinePrinter :: PrettyPrinter ()
newlinePrinter = do
  env <- get
  let tabs = concat $ replicate (indent env) "  "
  concatPrinter $ "\n" ++ tabs

castPP :: (IPyType b) => PrettyPrinter a -> PrettyPrinter b
castPP a = a >> return (def)

castUnitPP :: PrettyPrinter a -> PrettyPrinter ()
castUnitPP a = a >> return ()

--  iIf     :: (IExpr stmt) => stmt Bool -> stmt ()
--  iIfElse :: (IExpr stmt) => stmt Bool -> stmt () -> stmt ()
--  iWhile  :: (IExpr stmt) => stmt Bool -> stmt () -> stmt ()

instance IStatement PrettyPrinter where
  iIf expr a = (concatPrinter "if ") >> expr >> (concatPrinter ":")
            >> incIndentPrinter >> newlinePrinter
            >> a >> decIndentPrinter

  iIfElse expr a b = (concatPrinter "if ") >> expr >> (concatPrinter ":")
                  >> incIndentPrinter >> newlinePrinter
                  >> a >> decIndentPrinter >> newlinePrinter
                  >> (concatPrinter "else:")
                  >> incIndentPrinter >> newlinePrinter
                  >> b >> decIndentPrinter

  iWhile expr a = (concatPrinter "while ") >> expr >> (concatPrinter ":")
               >> incIndentPrinter >> newlinePrinter
               >> a >> decIndentPrinter

  iAssign s expr = castUnitPP $ (concatPrinter s) >> (concatPrinter " = ") >> expr
  iProcedure = castUnitPP
  iPrint a = (concatPrinter "print(") >> a >> (concatPrinter ")")
  iBreak = (concatPrinter "break")
  iContinue = (concatPrinter "continue")
  iNextStmt a b = a >> newlinePrinter >> b

instance IExpr PrettyPrinter where
  iOr  a b = a >> (concatPrinter " or ") >> b
  iAnd a b = a >> (concatPrinter " and ") >> b
  iNot = (>>) $ concatPrinter "not "

  iEq  a b = castPP $ a >> (concatPrinter " == ") >> b
  iNEq a b = castPP $ a >> (concatPrinter " != ") >> b
  iLT  a b = castPP $ a >> (concatPrinter " < ")  >> b
  iGT  a b = castPP $ a >> (concatPrinter " > ")  >> b
  iLTE a b = castPP $ a >> (concatPrinter " <= ") >> b
  iGTE a b = castPP $ a >> (concatPrinter " >= ") >> b

  iBitOr  a b = a >> (concatPrinter " | ") >> b
  iBitXor a b = a >> (concatPrinter " ^ ") >> b
  iBitAnd a b = a >> (concatPrinter " & ") >> b
  iLeftShift  a b = a >> (concatPrinter " << ") >> b
  iRightShift a b = a >> (concatPrinter " >> ") >> b

  iPlus     a b = a >> (concatPrinter " + ")  >> b
  iMinus    a b = a >> (concatPrinter " - ")  >> b
  iMult     a b = a >> (concatPrinter " * ")  >> b
  iFloatDiv a b = a >> (concatPrinter " / ")  >> b
  iDiv      a b = a >> (concatPrinter " // ") >> b
  iMod      a b = a >> (concatPrinter " % ")  >> b
  iPow      a b = a >> (concatPrinter "**")   >> b
  iUnarPlus  = (>>) $ concatPrinter "+"
  iUnarMinus = (>>) $ concatPrinter "-"

  iStrPlus a b = a >> (concatPrinter " + ") >> b

  iInput = castPP $ concatPrinter "input()"

  iValue x = castPP . concatPrinter . show $ x
  iVariable s = castPP . concatPrinter $ s

  iCastStr   a = castPP $ (concatPrinter "str")   >> iBrackets a
  iCastInt   a = castPP $ (concatPrinter "int")   >> iBrackets a
  iCastFloat a = castPP $ (concatPrinter "float") >> iBrackets a
  iCastBool  a = castPP $ (concatPrinter "bool")  >> iBrackets a
  iHidCastFloat = castPP

  iBrackets a = (concatPrinter "( ") >> a >>= (concatPrinterSafeType " )")

instance IPyScript PrettyPrinter

main = do
  inh <- openFile "py.py" ReadMode
  contents <- hGetContents inh
  case tfParse contents of
    Left s -> putStrLn s
    Right pyscript -> putStrLn $ asString $ execState pyscript $ PrintEnv "" 0
  -- case parse contents of
  --   Left s -> putStrLn s
  --   Right statements -> putStrLn $ show $ statements