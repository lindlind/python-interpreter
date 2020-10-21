{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods#-}

module ClassDef where

import Data.Typeable

class (Typeable t, Ord t, Show t) => IPyType t
instance IPyType Bool
instance IPyType Double
instance IPyType Integer
instance IPyType String

class IPyType t => IPyNumType t
instance IPyNumType Double
instance IPyNumType Integer

class IStatement stmt where
  iAssign    :: IExpr stmt => String -> stmt t -> stmt ()
  iProcedure :: IExpr stmt => stmt t -> stmt ()
  iPrint :: IExpr stmt => stmt t -> stmt ()
  iNextStmt :: stmt () -> stmt () -> stmt ()

class IExpr expr where
  iPlus :: IPyNumType t => expr t -> expr t -> expr t
  iMinus :: IPyNumType t => expr t -> expr t -> expr t
  iMult :: IPyNumType t => expr t -> expr t -> expr t
  iStrPlus :: expr String -> expr String -> expr String
  iInput :: expr String
  iVal :: IPyType t => t -> expr t
  iVarStr :: String -> expr String
  iVarInt :: String -> expr Integer
  iVarFloat :: String -> expr Double
  iVarBool :: String -> expr Bool
  iHidCastIntFloat :: expr Integer -> expr Double
  iBrackets :: IPyType t => expr t -> expr t

class (IStatement p, IExpr p) => IPyScript p