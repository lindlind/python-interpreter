{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module PrettyPrinter
  ( parseAndPrettyPrint
  , prettyPrint
  ) where

import ClassDef
  ( IExpr (..)
  , IStatement (..)
  , IPyNumType
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
  iIf :: PrettyPrinter Bool -> PrettyPrinter () 
         -> PrettyPrinter ()
  iIf expr a = (concatPrinter "if ") >> expr >> (concatPrinter ":")
            >> incIndentPrinter >> newlinePrinter
            >> a >> decIndentPrinter

  iIfElse :: PrettyPrinter Bool -> PrettyPrinter () -> PrettyPrinter () 
             -> PrettyPrinter ()
  iIfElse expr a b = (concatPrinter "if ") >> expr >> (concatPrinter ":")
                  >> incIndentPrinter >> newlinePrinter
                  >> a >> decIndentPrinter >> newlinePrinter
                  >> (concatPrinter "else:")
                  >> incIndentPrinter >> newlinePrinter
                  >> b >> decIndentPrinter

  iWhile :: PrettyPrinter Bool -> PrettyPrinter () 
            -> PrettyPrinter ()
  iWhile expr a = (concatPrinter "while ") >> expr >> (concatPrinter ":")
               >> incIndentPrinter >> newlinePrinter
               >> a >> decIndentPrinter

  iBreak :: PrettyPrinter () 
  iBreak = (concatPrinter "break")

  iContinue :: PrettyPrinter ()
  iContinue = (concatPrinter "continue")
  
  iDefFunc0 :: String -> PrettyPrinter () -> PrettyPrinter ()
  iDefFunc0 s a = (concatPrinter $ "def " ++ s ++ "(") 
               >> (concatPrinter "):") 
               >> incIndentPrinter >> newlinePrinter
               >> a >> decIndentPrinter

  iDefFunc1 :: String -> String -> PrettyPrinter () -> PrettyPrinter ()
  iDefFunc1 s s1 a = (concatPrinter $ "def " ++ s ++ "(" ++ s1) 
                  >> (concatPrinter "):") 
                  >> incIndentPrinter >> newlinePrinter
                  >> a >> decIndentPrinter

  iDefFunc2 :: String -> String -> String -> PrettyPrinter () -> PrettyPrinter ()
  iDefFunc2 s s1 s2 a = (concatPrinter $ "def " ++ s ++ "(" ++ s1 ++ ", " ++ s2) 
                     >> (concatPrinter "):") 
                     >> incIndentPrinter >> newlinePrinter
                     >> a >> decIndentPrinter
  
  iReturn :: IPyType t => PrettyPrinter t  -> PrettyPrinter ()
  iReturn expr = castUnitPP $ (concatPrinter "return ") >> expr

  iAssign    :: IPyType t => String -> PrettyPrinter t  -> PrettyPrinter ()
  iAssign s expr = castUnitPP $ (concatPrinter $ s ++ " = ") >> expr
  iProcedure :: IPyType t => PrettyPrinter t  -> PrettyPrinter ()
  iProcedure = castUnitPP
  iPrint     :: IPyType t => PrettyPrinter t  -> PrettyPrinter ()
  iPrint a = (concatPrinter "print(") >> a >> (concatPrinter ")")
  iNextStmt  :: PrettyPrinter () -> PrettyPrinter () -> PrettyPrinter ()
  iNextStmt a b = a >> newlinePrinter >> b

instance IExpr PrettyPrinter where
  iOr  :: PrettyPrinter Bool -> PrettyPrinter Bool -> PrettyPrinter Bool
  iOr  a b = a >> (concatPrinter " or ") >> b
  iAnd :: PrettyPrinter Bool -> PrettyPrinter Bool -> PrettyPrinter Bool
  iAnd a b = a >> (concatPrinter " and ") >> b
  iNot :: PrettyPrinter Bool -> PrettyPrinter Bool
  iNot = (>>) $ concatPrinter "not "

  iEq  :: IPyType t => PrettyPrinter t -> PrettyPrinter t -> PrettyPrinter Bool
  iEq  a b = castPP $ a >> (concatPrinter " == ") >> b
  iNEq :: IPyType t => PrettyPrinter t -> PrettyPrinter t -> PrettyPrinter Bool
  iNEq a b = castPP $ a >> (concatPrinter " != ") >> b
  iLT  :: IPyType t => PrettyPrinter t -> PrettyPrinter t -> PrettyPrinter Bool
  iLT  a b = castPP $ a >> (concatPrinter " < ")  >> b
  iGT  :: IPyType t => PrettyPrinter t -> PrettyPrinter t -> PrettyPrinter Bool
  iGT  a b = castPP $ a >> (concatPrinter " > ")  >> b
  iLTE :: IPyType t => PrettyPrinter t -> PrettyPrinter t -> PrettyPrinter Bool
  iLTE a b = castPP $ a >> (concatPrinter " <= ") >> b
  iGTE :: IPyType t => PrettyPrinter t -> PrettyPrinter t -> PrettyPrinter Bool
  iGTE a b = castPP $ a >> (concatPrinter " >= ") >> b

  iBitOr  :: PrettyPrinter Integer -> PrettyPrinter Integer -> PrettyPrinter Integer
  iBitOr  a b = a >> (concatPrinter " | ") >> b
  iBitXor :: PrettyPrinter Integer -> PrettyPrinter Integer -> PrettyPrinter Integer
  iBitXor a b = a >> (concatPrinter " ^ ") >> b
  iBitAnd :: PrettyPrinter Integer -> PrettyPrinter Integer -> PrettyPrinter Integer
  iBitAnd a b = a >> (concatPrinter " & ") >> b
  iLeftShift  :: PrettyPrinter Integer -> PrettyPrinter Integer -> PrettyPrinter Integer
  iLeftShift  a b = a >> (concatPrinter " << ") >> b
  iRightShift :: PrettyPrinter Integer -> PrettyPrinter Integer -> PrettyPrinter Integer
  iRightShift a b = a >> (concatPrinter " >> ") >> b

  iPlus :: IPyNumType t => PrettyPrinter t -> PrettyPrinter t -> PrettyPrinter t
  iPlus a b = a >> (concatPrinter " + ")  >> b
  iMinus :: IPyNumType t => PrettyPrinter t -> PrettyPrinter t -> PrettyPrinter t
  iMinus a b = a >> (concatPrinter " - ")  >> b
  iMult :: IPyNumType t => PrettyPrinter t -> PrettyPrinter t -> PrettyPrinter t
  iMult a b = a >> (concatPrinter " * ")  >> b
  iFloatDiv :: PrettyPrinter Double -> PrettyPrinter Double -> PrettyPrinter Double
  iFloatDiv a b = a >> (concatPrinter " / ")  >> b
  iDiv :: PrettyPrinter Integer -> PrettyPrinter Integer -> PrettyPrinter Integer
  iDiv a b = a >> (concatPrinter " // ") >> b
  iMod :: PrettyPrinter Integer -> PrettyPrinter Integer -> PrettyPrinter Integer
  iMod a b = a >> (concatPrinter " % ")  >> b
  iPow :: PrettyPrinter Double -> PrettyPrinter Double -> PrettyPrinter Double
  iPow a b = a >> (concatPrinter "**")   >> b
  iUnarPlus :: IPyNumType t => PrettyPrinter t -> PrettyPrinter t
  iUnarPlus = (>>) $ concatPrinter "+"
  iUnarMinus :: IPyNumType t => PrettyPrinter t -> PrettyPrinter t
  iUnarMinus = (>>) $ concatPrinter "-"

  iStrPlus :: PrettyPrinter String -> PrettyPrinter String 
              -> PrettyPrinter String
  iStrPlus a b = a >> (concatPrinter " + ") >> b

  iSlice1 :: PrettyPrinter String 
             -> PrettyPrinter Integer 
             -> PrettyPrinter String
  iSlice1 s a = castPP $ s >> (concatPrinter "[") 
                      >> a >> (concatPrinter "]")

  iSlice2 :: PrettyPrinter String 
             -> PrettyPrinter Integer -> PrettyPrinter Integer 
             -> PrettyPrinter String
  iSlice2 s a b = castPP $ s >> (concatPrinter "[") 
                        >> a >> (concatPrinter ":") 
                        >> b >> (concatPrinter "]")
 
  iCallFunc0 :: IPyType t => String -> PrettyPrinter t
  iCallFunc0 s = castPP $ (concatPrinter $ s ++ "(") 
                           >> (concatPrinter ")")
  iCallFunc1 :: IPyType t 
                => String -> PrettyPrinter () 
                -> PrettyPrinter t
  iCallFunc1 s a = castPP $ (concatPrinter $ s ++ "(") 
                           >> a >> (concatPrinter ")")
  iCallFunc2 :: IPyType t 
                => String -> PrettyPrinter () -> PrettyPrinter () 
                -> PrettyPrinter t
  iCallFunc2 s a b = castPP $ (concatPrinter $ s ++ "(") 
                           >> a >> (concatPrinter ", ") 
                           >> b >> (concatPrinter ")")

  iPushToStack :: IPyType t => PrettyPrinter t -> PrettyPrinter ()
  iPushToStack = castUnitPP

  iInput :: PrettyPrinter String
  iInput = castPP $ concatPrinter "input()"

  iValue :: IPyType t => t -> PrettyPrinter t
  iValue x = castPP . concatPrinter . show $ x
  iVariable :: IPyType t => String -> PrettyPrinter t
  iVariable s = castPP . concatPrinter $ s

  iCastStr   :: IPyType t => PrettyPrinter t -> PrettyPrinter String
  iCastStr   a = castPP $ (concatPrinter "str")   >> iBrackets a
  iCastInt   :: IPyType t => PrettyPrinter t -> PrettyPrinter Integer
  iCastInt   a = castPP $ (concatPrinter "int")   >> iBrackets a
  iCastFloat :: IPyType t => PrettyPrinter t -> PrettyPrinter Double
  iCastFloat a = castPP $ (concatPrinter "float") >> iBrackets a
  iCastBool  :: IPyType t => PrettyPrinter t -> PrettyPrinter Bool
  iCastBool  a = castPP $ (concatPrinter "bool")  >> iBrackets a
  iHidCastFloat :: IPyNumType t => PrettyPrinter t -> PrettyPrinter Double
  iHidCastFloat = castPP

  iBrackets :: IPyType t => PrettyPrinter t -> PrettyPrinter t
  iBrackets a = castPP $ (concatPrinter "(") >> a >> (concatPrinter ")")

instance IPyScript PrettyPrinter

-- | Function gets python code as tagless final eDSL
-- and shows it in pretty form.
prettyPrint :: PrettyPrinter () -> String
prettyPrint pyscript = asString $ execState pyscript initPrinterEnv

-- | Function gets python code as string,
-- converts it to tagless final eDSL and shows it in pretty form.
-- It uses Retyper's tfParse and prettyPrint.
parseAndPrettyPrint :: String -> String
parseAndPrettyPrint string = 
  case runExcept $ tfParse string of
    Left err -> show err
    Right pyscript -> prettyPrint pyscript