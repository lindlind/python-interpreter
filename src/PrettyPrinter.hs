{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinter where

import ClassDef
  ( IExpr (..)
  , IStatement (..)
  , IPyScript
  , IPyType
  , def
  )
import Retyper
  ( tfParse
  )

import Control.Monad.Except 
  ( runExcept
  )
import Control.Monad.State.Strict
  ( State
  , get
  , modify
  , execState
  )
import System.IO

data PrinterEnviroment 
  = PrintEnv 
  { asString :: String
  , indent :: Int
  }

initPrinterEnv :: PrinterEnviroment
initPrinterEnv 
  = PrintEnv 
  { asString = ""
  , indent = 0
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

  iBreak = (concatPrinter "break")
  iContinue = (concatPrinter "continue")
  
  iDefFunc0 s a = (concatPrinter $ "def " ++ s ++ "(") 
               >> (concatPrinter "):") 
               >> incIndentPrinter >> newlinePrinter
               >> a >> decIndentPrinter

  iDefFunc1 s s1 a = (concatPrinter $ "def " ++ s ++ "(" ++ s1) 
                  >> (concatPrinter "):") 
                  >> incIndentPrinter >> newlinePrinter
                  >> a >> decIndentPrinter

  iDefFunc2 s s1 s2 a = (concatPrinter $ "def " ++ s ++ "(" ++ s1 ++ ", " ++ s2) 
                     >> (concatPrinter "):") 
                     >> incIndentPrinter >> newlinePrinter
                     >> a >> decIndentPrinter

  iReturn expr = castUnitPP $ (concatPrinter "return ") >> expr

  iAssign s expr = castUnitPP $ (concatPrinter $ s ++ " = ") >> expr
  iProcedure = castUnitPP
  iPrint a = (concatPrinter "print(") >> a >> (concatPrinter ")")
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

  iSlice1 s a = castPP $ s >> (concatPrinter "[") 
                      >> a >> (concatPrinter "]")

  iSlice2 s a b = castPP $ s >> (concatPrinter "[") 
                        >> a >> (concatPrinter ":") 
                        >> b >> (concatPrinter "]")

  iCallFunc0 s     = castPP $ (concatPrinter $ s ++ "(") 
                           >> (concatPrinter ")")
  iCallFunc1 s a   = castPP $ (concatPrinter $ s ++ "(") 
                           >> a >> (concatPrinter ")")
  iCallFunc2 s a b = castPP $ (concatPrinter $ s ++ "(") 
                           >> a >> (concatPrinter ", ") 
                           >> b >> (concatPrinter ")")

  iPushToStack = castUnitPP

  iInput = castPP $ concatPrinter "input()"

  iValue x = castPP . concatPrinter . show $ x
  iVariable s = castPP . concatPrinter $ s

  iCastStr   a = castPP $ (concatPrinter "str")   >> iBrackets a
  iCastInt   a = castPP $ (concatPrinter "int")   >> iBrackets a
  iCastFloat a = castPP $ (concatPrinter "float") >> iBrackets a
  iCastBool  a = castPP $ (concatPrinter "bool")  >> iBrackets a
  iHidCastFloat = castPP

  iBrackets a = castPP $ (concatPrinter "(") >> a >> (concatPrinter ")")

instance IPyScript PrettyPrinter

-- | Function gets python code as tagless final eDSL
-- and prints it in pretty form.
prettyPrint :: PrettyPrinter () -> String
prettyPrint pyscript = asString $ execState pyscript initPrinterEnv

-- | Function gets python code as string,
-- converts it to tagless final eDSL and prints it in pretty form.
-- It uses Retyper's tfParse and prettyPrint.
parseAndPrettyPrint :: String -> String
parseAndPrettyPrint string = 
  case runExcept $ tfParse string of
    Left error -> show error
    Right pyscript -> prettyPrint pyscript