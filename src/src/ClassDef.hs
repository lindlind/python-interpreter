{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances       #-}

module ClassDef 
  ( IExpr (..)
  , IStatement (..)
  , IPyScript
  , IPyType
  , SimpleCast (..)
  , ParseException (..)
  , PyType (..)
  , def
  ) where

import Lexer 
  ( AlexPosn (..)
  )

import Data.List
  ( intercalate
  )
import Data.Typeable
  ( Typeable
  )

class    Default t       where def :: t

instance Default String  where def = ""
instance Default Integer where def = 0
instance Default Double  where def = 0
instance Default Bool    where def = True

class SimpleCast t where
  castToStr     :: t -> String
  castToInteger :: t -> Integer
  castToDouble  :: t -> Double
  castToBool    :: t -> Bool

instance SimpleCast String where
  castToStr     = id
  castToInteger = read
  castToDouble  = read
  castToBool    = read

instance SimpleCast Integer where
  castToStr     = show
  castToInteger = id
  castToDouble  = realToFrac
  castToBool    = (0 /=)

instance SimpleCast Double where
  castToStr     = show
  castToInteger = round
  castToDouble  = id
  castToBool    = (0 /=)

instance SimpleCast Bool where
  castToStr     = show
  castToInteger = toInteger . fromEnum
  castToDouble  = realToFrac . fromEnum
  castToBool    = id

class ( Typeable   t
      , Default    t
      , SimpleCast t
      , Ord        t
      , Show       t
      )              => IPyType t

instance IPyType Bool
instance IPyType Double
instance IPyType Integer
instance IPyType String

class (IPyType t, Num t) => IPyNumType t
instance IPyNumType Double
instance IPyNumType Integer

data PyType where
  PyType :: IPyType a => a -> PyType

class IStatement stmt where
  iIf     :: (IExpr stmt) => stmt Bool -> stmt () -> stmt ()
  iIfElse :: (IExpr stmt) => stmt Bool -> stmt () -> stmt () -> stmt ()
  iWhile  :: (IExpr stmt) => stmt Bool -> stmt () -> stmt ()
  iBreak    :: stmt ()
  iContinue :: stmt ()

  iDefFunc0 :: String                     -> stmt () -> stmt ()
  iDefFunc1 :: String -> String           -> stmt () -> stmt ()
  iDefFunc2 :: String -> String -> String -> stmt () -> stmt ()
  iReturn :: (IExpr stmt, IPyType t) => stmt t  -> stmt ()

  iAssign    :: (IExpr stmt, IPyType t) => String -> stmt t  -> stmt ()
  iProcedure :: (IExpr stmt, IPyType t) =>           stmt t  -> stmt ()
  iPrint     :: (IExpr stmt, IPyType t) =>           stmt t  -> stmt ()
  iNextStmt  :: stmt () -> stmt () -> stmt ()

class IExpr expr where
  iOr  :: expr Bool -> expr Bool -> expr Bool
  iAnd :: expr Bool -> expr Bool -> expr Bool
  iNot :: expr Bool -> expr Bool

  iEq  :: IPyType t => expr t -> expr t -> expr Bool
  iNEq :: IPyType t => expr t -> expr t -> expr Bool
  iLT  :: IPyType t => expr t -> expr t -> expr Bool
  iGT  :: IPyType t => expr t -> expr t -> expr Bool
  iLTE :: IPyType t => expr t -> expr t -> expr Bool
  iGTE :: IPyType t => expr t -> expr t -> expr Bool

  iBitOr  :: expr Integer -> expr Integer -> expr Integer
  iBitXor :: expr Integer -> expr Integer -> expr Integer
  iBitAnd :: expr Integer -> expr Integer -> expr Integer
  iLeftShift  :: expr Integer -> expr Integer -> expr Integer
  iRightShift :: expr Integer -> expr Integer -> expr Integer

  iPlus     :: IPyNumType t => expr t -> expr t -> expr t
  iMinus    :: IPyNumType t => expr t -> expr t -> expr t
  iMult     :: IPyNumType t => expr t -> expr t -> expr t
  iFloatDiv :: expr Double -> expr Double -> expr Double
  iDiv      :: expr Integer -> expr Integer -> expr Integer
  iMod      :: expr Integer -> expr Integer -> expr Integer
  iPow      :: expr Double -> expr Double -> expr Double
  iUnarPlus  :: IPyNumType t => expr t -> expr t
  iUnarMinus :: IPyNumType t => expr t -> expr t

  iStrPlus :: expr String -> expr String -> expr String
  iSlice1 :: expr String -> expr Integer                 -> expr String
  iSlice2 :: expr String -> expr Integer -> expr Integer -> expr String

  iCallFunc0 :: IPyType t => String                       -> expr t
  iCallFunc1 :: IPyType t => String -> expr ()            -> expr t
  iCallFunc2 :: IPyType t => String -> expr () -> expr () -> expr t
  iPushToStack :: IPyType t => expr t -> expr ()

  iInput :: expr String

  iValue :: IPyType t => t -> expr t
  iVariable :: IPyType t => String -> expr t

  iCastStr   :: IPyType t => expr t -> expr String
  iCastInt   :: IPyType t => expr t -> expr Integer
  iCastFloat :: IPyType t => expr t -> expr Double
  iCastBool  :: IPyType t => expr t -> expr Bool
  iHidCastFloat :: IPyNumType t => expr t -> expr Double

  iBrackets :: IPyType t => expr t -> expr t

class (IStatement p, IExpr p) => IPyScript p

data ParseException = CommonParserError String
                    | TypeError [String] String AlexPosn
                    | OpTypeError String AlexPosn
                    | CastTypeError AlexPosn
                    | VarNotDefinedError String AlexPosn
                    | FunctionNotDefinedError String AlexPosn
                    | FunctionRedefinitionError String
                    | FunctionArgsTypeError String String AlexPosn
                    | FunctionArgsCountError String AlexPosn

instance Show ParseException where
  show (CommonParserError s) = s
  show (TypeError l stmt (AlexPn _ line column)) 
       = "TypeError: "
      ++ "wrong type in " ++ stmt ++ "; "
      ++ "line " ++ (show line) ++ ", "
      ++ "column " ++ (show column) ++ ", "
      ++ "possible types: " ++ intercalate "," l
  show (OpTypeError op (AlexPn _ line column)) 
       = "OperationTypeError: "
      ++ "wrong type in operation '" ++ op ++ "'; "
      ++ "line " ++ (show line) ++ ", "
      ++ "column " ++ (show column)
  show (CastTypeError (AlexPn _ line column)) 
       = "CastTypeError: "
      ++ "wrong cast type;"
      ++ "line " ++ (show line) ++ ", "
      ++ "column " ++ (show column)
  show (VarNotDefinedError name (AlexPn _ line column)) 
       = "VarNotDefinedError: "
      ++ "wrong variable '" ++ name ++ "'; "
      ++ "line " ++ (show line) ++ ", "
      ++ "column " ++ (show column)
  show (FunctionNotDefinedError name (AlexPn _ line column)) 
       = "FunctionNotDefinedError: "
      ++ "wrong function '" ++ name ++ "'; "
      ++ "line " ++ (show line) ++ ", "
      ++ "column " ++ (show column)
  show (FunctionRedefinitionError name) 
       = "FunctionRedefinitionError: "
      ++ "multiple definition of function '" ++ name ++ "'"
  show (FunctionArgsTypeError name order (AlexPn _ line column)) 
       = "FunctionArgsTypeError: "
      ++ "wrong type of " ++ order ++ " variable "
      ++ "in function '" ++ name ++ "'; "
      ++ "line " ++ (show line) ++ ", "
      ++ "column " ++ (show column)
  show (FunctionArgsCountError name (AlexPn _ line column)) 
       = "FunctionArgsCountError: "
      ++ "wrong number of arguments in function '" ++ name ++ "'; "
      ++ "line " ++ (show line) ++ ", "
      ++ "column " ++ (show column)