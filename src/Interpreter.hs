{-# LANGUAGE FlexibleInstances #-}

module Interpreter where

import ClassDef
  ( IExpr (..)
  , IStatement (..)
  , IPyScript
  , IPyType
  , SimpleCast (..)
  , PyType (..)
  )
import Retyper
  ( tfParse
  )

import Control.Monad.ST
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Typeable
import Data.Bits
import Data.IORef
import System.IO

data Environment 
  = Env 
  { varRefs :: Map.Map String (IORef PyType)
  , funcs :: Map.Map String ([String], Interpreter ())
  , stackArgs :: [IORef PyType]
  , loopFlag :: Bool
  , funcFlag :: Bool
  , breakFlag :: Bool
  , continueFlag :: Bool
  , returnFlag :: Bool
  }

initEnvironment :: Environment
initEnvironment 
  = Env 
  { varRefs = Map.empty
  , funcs = Map.empty
  , stackArgs = []
  , loopFlag = False
  , funcFlag = False
  , breakFlag = False
  , continueFlag = False
  , returnFlag = False
  }

updateRefMap :: (Map.Map String (IORef PyType) ->  Map.Map String (IORef PyType))
                -> Environment -> Environment
updateRefMap f env@Env{ varRefs = mp } = env { varRefs = f mp }

updateFuncsMap :: (  Map.Map String ([String], Interpreter ()) 
                  -> Map.Map String ([String], Interpreter ())
                  ) -> Environment -> Environment
updateFuncsMap f env@Env{ funcs = mp } = env { funcs = f mp }

pushStack :: IORef PyType -> Environment -> Environment
pushStack ref env@Env{ stackArgs = stk } = env { stackArgs = ref : stk }

popStack :: Environment -> Environment
popStack env@Env{ stackArgs = ref : stk } = env { stackArgs = stk }

getHeadStack :: Environment -> IORef PyType
getHeadStack env@Env{ stackArgs = ref : stk } = ref

updateLoopFlag :: Bool -> Environment -> Environment
updateLoopFlag newFlag env@Env{ loopFlag = flag } = env { loopFlag = newFlag }

updateFuncFlag :: Bool -> Environment -> Environment
updateFuncFlag newFlag env@Env{ funcFlag = flag } = env { funcFlag = newFlag }

updateBreakFlag :: Bool -> Environment -> Environment
updateBreakFlag newFlag env@Env{ breakFlag = flag } = env { breakFlag = newFlag }

updateContinueFlag :: Bool -> Environment -> Environment
updateContinueFlag newFlag env@Env{ continueFlag = flag } = env { continueFlag = newFlag }

updateReturnFlag :: Bool -> Environment -> Environment
updateReturnFlag newFlag env@Env{ returnFlag = flag } = env { returnFlag = newFlag }

type Interpreter = StateT Environment IO

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
    env <- get
    cond <- expr
    if cond && not (returnFlag env)
    then do
      let oldLoopFlag = loopFlag env
      modify $ updateLoopFlag True
      thn
      modify $ updateContinueFlag False
      envThn <- get
      if breakFlag envThn
      then modify $ updateBreakFlag False
      else iWhile expr thn
      modify $ updateLoopFlag oldLoopFlag
    else return ()

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

  iDefFunc0 name a = 
    modify $ updateFuncsMap (Map.insert name ([], a))
  iDefFunc1 name arg1 a = 
    modify $ updateFuncsMap (Map.insert name ([arg1], a))
  iDefFunc2 name arg1 arg2 a = 
    modify $ updateFuncsMap (Map.insert name ([arg1, arg2], a))

  iReturn expr = do
    env <- get
    if not $ funcFlag env
      then error "InterpretError: 'return' outside function"
      else do
        iAssign "~funcResult~" expr
        modify $ updateReturnFlag True

  iAssign name expr = do
    env <- get
    v <- expr
    ref <- lift $ newIORef $ PyType v
    modify $ updateRefMap (Map.insert name ref)

  iProcedure expr = return ()

  iPrint a = do
    v <- a
    liftIO $ putStrLn $ show v

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
  iFloatDiv a b = (/) <$> iCastFloat a <*> iCastFloat b
  iDiv      a b = div <$> a <*> b
  iMod      a b = mod <$> a <*> b
  iPow      a b = (**) <$> iCastFloat a <*> iCastFloat b
  iUnarPlus = id
  iUnarMinus a = (0 -) <$> a

  iStrPlus a b = (++) <$> a <*> b
  iSlice1 s a = iSlice2 s a $ (+1) <$> a
  iSlice2 s a b = substr <$> a <*> b <*> s

  iCallFunc0 name = do
    env <- get

    let (_, funcBlock) = funcs env Map.! name
    put initEnvironment
    modify $ updateFuncsMap (\_ -> funcs env)
    modify $ updateFuncFlag True

    funcBlock
    funcEnv <- get
    res <- iVariable "~funcResult~"

    put env
    return res

  iCallFunc1 name a = do
    a
    envA <- get
    let ref1 = getHeadStack envA
    modify $ popStack
    env <- get

    let (arg1 : _, funcBlock) = funcs env Map.! name
    put initEnvironment
    modify $ updateFuncsMap (\_ -> funcs env)
    modify $ updateRefMap (Map.insert arg1 ref1)
    modify $ updateFuncFlag True

    funcBlock
    funcEnv <- get
    res <- iVariable "~funcResult~"

    put env
    return res

  iCallFunc2 name a b = do
    a
    envA <- get
    let ref1 = getHeadStack envA
    modify $ popStack
    b
    envB <- get
    let ref2 = getHeadStack envB
    modify $ popStack
    env <- get

    let (arg1 :arg2 : _, funcBlock) = funcs env Map.! name
    put initEnvironment
    modify $ updateFuncsMap (\_ -> funcs env)
    modify $ updateRefMap (Map.insert arg1 ref1)
    modify $ updateRefMap (Map.insert arg2 ref2)
    modify $ updateFuncFlag True

    funcBlock
    funcEnv <- get
    res <- iVariable "~funcResult~"
    
    put env
    return res

  iPushToStack a = do
    v <- a
    ref <- lift $ newIORef $ PyType v
    modify $ pushStack ref

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

substr :: Integer -> Integer -> String -> String
substr a b = take (fromIntegral $ b - a) . drop (fromIntegral a)

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
    Left s -> putStrLn $ show s
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