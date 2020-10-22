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

castUnitPP :: PrettyPrinter a -> PrettyPrinter ()
castUnitPP a = a >> return ()

castStrPP :: PrettyPrinter a -> PrettyPrinter String
castStrPP a = a >> return ""

castIntPP :: PrettyPrinter a -> PrettyPrinter Integer
castIntPP a = a >> return (0 :: Integer)

castFloatPP :: PrettyPrinter a -> PrettyPrinter Double
castFloatPP a = a >> return (0 :: Double)

castBoolPP :: PrettyPrinter a -> PrettyPrinter Bool
castBoolPP a = a >> return True

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
  iNextStmt a b = a >> newlinePrinter >> b

instance IExpr PrettyPrinter where
  iOr  a b = a >> (concatPrinter " or ") >> b
  iAnd a b = a >> (concatPrinter " and ") >> b
  iNot = (>>) $ concatPrinter "not "
  iEq  a b = castBoolPP $ a >> (concatPrinter " == ") >> b
  iNEq a b = castBoolPP $ a >> (concatPrinter " != ") >> b
  iLT  a b = castBoolPP $ a >> (concatPrinter " < ")  >> b
  iGT  a b = castBoolPP $ a >> (concatPrinter " > ")  >> b
  iLTE a b = castBoolPP $ a >> (concatPrinter " <= ") >> b
  iGTE a b = castBoolPP $ a >> (concatPrinter " >= ") >> b
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
  iInput = castStrPP $ concatPrinter "input()"
  iValueStr   x = castStrPP   . concatPrinter . show $ x
  iValueInt   x = castIntPP   . concatPrinter . show $ x
  iValueFloat x = castFloatPP . concatPrinter . show $ x
  iValueBool  x = castBoolPP  . concatPrinter . show $ x
  iVarStr   s = castStrPP   . concatPrinter $ s
  iVarInt   s = castIntPP   . concatPrinter $ s
  iVarFloat s = castFloatPP . concatPrinter $ s
  iVarBool  s = castBoolPP  . concatPrinter $ s
  iCastStr   a = castStrPP   $ (concatPrinter "str")   >> iBrackets a
  iCastInt   a = castIntPP   $ (concatPrinter "int")   >> iBrackets a
  iCastFloat a = castFloatPP $ (concatPrinter "float") >> iBrackets a
  iCastBool  a = castBoolPP  $ (concatPrinter "bool")  >> iBrackets a
  iHidCastFloat = castFloatPP
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