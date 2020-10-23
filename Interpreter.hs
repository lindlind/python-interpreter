{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter where

import ClassDef
import Retyper

import Control.Monad.ST
import Data.IORef
import System.IO
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Typeable
import Data.Bits

data Environment = Env { varRefs :: Map.Map String (IORef PyType)
                       , loopFlag :: Bool
                       , breakFlag :: Bool
                       , continueFlag :: Bool
                       }

initEnvironment :: Environment
initEnvironment = Env { varRefs = Map.empty
                      , loopFlag = False
                      , breakFlag = False
                      , continueFlag = False
                      }

type Interpreter = StateT Environment IO

updateRefMap :: (Map.Map String (IORef PyType) ->  Map.Map String (IORef PyType))
                -> Environment -> Environment
updateRefMap f env@Env{ varRefs = rs } = env { varRefs = f rs }

updateLoopFlag :: Bool -> Environment -> Environment
updateLoopFlag newFlag env@Env{ loopFlag = flag } = env { loopFlag = newFlag }

updateBreakFlag :: Bool -> Environment -> Environment
updateBreakFlag newFlag env@Env{ breakFlag = flag } = env { breakFlag = newFlag }

updateContinueFlag :: Bool -> Environment -> Environment
updateContinueFlag newFlag env@Env{ continueFlag = flag } = env { continueFlag = newFlag }

instance IStatement Interpreter where
  iIf expr thn = do
    cond <- expr
    if cond
    then thn
    else return ()

  iIfElse expr thn els = do
    cond <- expr
    if cond
    then thn
    else els

  iWhile expr thn = do
    cond <- expr
    if cond
    then do
      modify $ updateLoopFlag True
      thn
      modify $ updateContinueFlag False
      env <- get
      if breakFlag env
      then modify $ updateBreakFlag False
      else iWhile expr thn
      modify $ updateLoopFlag False
    else return ()

  iAssign name expr = do
    env <- get
    v <- expr
    ref <- lift $ newIORef $ PyType v
    modify $ updateRefMap (Map.insert name ref)

  iProcedure expr = return ()

  iPrint a = do
    v <- a
    liftIO $ putStrLn $ show v

  iBreak = do
    env <- get
    if not $ loopFlag env
      then error "InterpretError: 'break' outside loop"
      else modify $ updateBreakFlag True

  iContinue = do
    env <- get
    if not $ loopFlag env
      then error "InterpretError: 'continue' outside loop"
      else modify $ updateContinueFlag True

  iNextStmt a b = do
    a
    env <- get
    if loopFlag env && (breakFlag env || continueFlag env)
    then return ()
    else b

instance IExpr Interpreter where
  iOr  a b = (||) <$> a <*> b
  iAnd a b = (&&) <$> a <*> b
  iNot a   =  not <$> a

  iEq  a b = (==) <$> a <*> b
  iNEq a b = (/=) <$> a <*> b
  iLT  a b = (<)  <$> a <*> b
  iGT  a b = (>)  <$> a <*> b
  iLTE a b = (<=) <$> a <*> b
  iGTE a b = (>=) <$> a <*> b

  iBitOr  a b = (.|.) <$> a <*> b
  iBitXor a b = (xor) <$> a <*> b
  iBitAnd a b = (.&.) <$> a <*> b
  iLeftShift  a b = shift <$> a <*> (fromIntegral <$> b)
  iRightShift a b = shift <$> a <*> (fromIntegral <$> iUnarMinus b)

  iPlus     a b = (+) <$> a <*> b
  iMinus    a b = (-) <$> a <*> b
  iMult     a b = (*) <$> a <*> b
  iFloatDiv a b = (/) <$> a <*> b
  iDiv      a b = div <$> a <*> b
  iMod      a b = mod <$> a <*> b
  iPow      a b = (**) <$> a <*> b
  iUnarPlus  a = a
  iUnarMinus a = (0-) <$> a

  iStrPlus a b = (++) <$> a <*> b

  iInput = liftIO $ getLine

  iValue = return
  iVariable name = do
    env <- get
    let ref = varRefs env Map.! name
    (PyType val) <- lift $ readIORef ref
    let val' = case cast val of
                 (Just b) -> b
    return val'

  iCastStr   a = castToStr     <$> a
  iCastInt   a = castToInteger <$> a
  iCastFloat a = castToDouble  <$> a
  iCastBool  a = castToBool    <$> a
  iHidCastFloat = iCastFloat

  iBrackets = id

instance IPyScript Interpreter

-- qq :: ST s PyType -> Maybe String
-- qq q = runST $ fromPyTypeStr <$> q

qqq :: () -> String
qqq q = "Nothing"

interpretScript :: Interpreter a -> IO a
interpretScript script = evalStateT script initEnvironment

eitherRight :: Either a b -> b
eitherRight (Right x) = x

main = do
  inh <- openFile "py.py" ReadMode
  contents <- hGetContents inh
  case tfParse contents of
    Left s -> putStrLn s
    Right pyscript -> do
      interpretScript $ pyscript
      -- let ref = varRefs env Map.! "result"
      -- let j = qq $ readSTRef ref
      -- case j of
      --   (Just jj) -> putStrLn jj
      --   _         -> putStrLn "Nothing"

-- eitherRight :: Either a b -> b
-- eitherRight (Right x) = x

-- main = do
--   inh <- openFile "py.py" ReadMode
--   contents <- hGetContents inh
--   let (r, env) = runState (eitherRight $ tfParse contents)
--                     $ Env { varRefs = Map.empty }
--   let ref = varRefs env Map.! "result"
--   let j = fromPyTypeStr $ runST $ readSTRef ref
--   case j of
--     (Just jj) -> putStrLn jj
--     _         -> putStrLn "Nothing"
