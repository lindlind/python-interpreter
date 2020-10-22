{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods#-}

module ClassDef where

import Data.Bits
import Data.Typeable

class (Typeable t, Ord t, Show t) => IPyType t
instance IPyType Bool
instance IPyType Double
instance IPyType Integer
instance IPyType String

class (IPyType t, Num t) => IPyNumType t
instance IPyNumType Double
instance IPyNumType Integer

class IStatement stmt where
  iIf     :: (IExpr stmt) => stmt Bool -> stmt () -> stmt ()
  iIfElse :: (IExpr stmt) => stmt Bool -> stmt () -> stmt () -> stmt ()
  iWhile  :: (IExpr stmt) => stmt Bool -> stmt () -> stmt ()
  iAssign    :: (IExpr stmt, IPyType t) => String -> stmt t  -> stmt ()
  iProcedure :: (IExpr stmt, IPyType t) =>           stmt t  -> stmt ()
  iPrint     :: (IExpr stmt, IPyType t) =>           stmt t  -> stmt ()
  iNextStmt  :: (IExpr stmt) => stmt () -> stmt () -> stmt ()

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
  iInput :: expr String
  iValueStr   :: String -> expr String
  iValueInt   :: Integer -> expr Integer
  iValueFloat :: Double -> expr Double
  iValueBool  :: Bool -> expr Bool
  iVarStr   :: String -> expr String
  iVarInt   :: String -> expr Integer
  iVarFloat :: String -> expr Double
  iVarBool  :: String -> expr Bool
  iCastStr   :: IPyType t => expr t -> expr String
  iCastInt   :: IPyType t => expr t -> expr Integer
  iCastFloat :: IPyType t => expr t -> expr Double
  iCastBool  :: IPyType t => expr t -> expr Bool
  iHidCastFloat :: IPyType t => expr t -> expr Double
  iBrackets :: IPyType t => expr t -> expr t

class (IStatement p, IExpr p) => IPyScript p