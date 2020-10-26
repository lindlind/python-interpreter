{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module Interpreter
  ( getVariablesFromEnvironment
  , interpret
  , parseAndGetVariables
  , parseAndInterpret
  ) where

import ClassDef
  ( IExpr (..)
  , IStatement (..)
  , IPyNumType
  , IPyScript
  , IPyType
  , SimpleCast (..)
  , PyType (..)
  )
import Retyper
  ( tfParse
  )

import Control.Monad.Except 
  ( runExcept
  )
import Control.Monad.State.Strict
  ( StateT
  , evalStateT
  , execStateT
  , get
  , lift
  , liftIO
  , modify
  , put
  )
import qualified Data.Map.Strict as Map
  ( Map
  , (!)
  , empty
  , insert
  )
import Data.Bits
  ( (.|.)
  , (.&.)
  , shift
  ,  xor
  )
import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  )
import Data.Typeable
  ( cast
  )

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
popStack env@Env{ stackArgs = _ : stk } = env { stackArgs = stk }
popStack _ = error "popStack: impossible case"

getHeadStack :: Environment -> IORef PyType
getHeadStack Env{ stackArgs = ref : _ } = ref
getHeadStack _ = error "getHeadStack: impossible case"

updateLoopFlag :: Bool -> Environment -> Environment
updateLoopFlag newFlag env@Env{ loopFlag = _ } = env { loopFlag = newFlag }

updateFuncFlag :: Bool -> Environment -> Environment
updateFuncFlag newFlag env@Env{ funcFlag = _ } = env { funcFlag = newFlag }

updateBreakFlag :: Bool -> Environment -> Environment
updateBreakFlag newFlag env@Env{ breakFlag = _ } = env { breakFlag = newFlag }

updateContinueFlag :: Bool -> Environment -> Environment
updateContinueFlag newFlag env@Env{ continueFlag = _ } = env { continueFlag = newFlag }

updateReturnFlag :: Bool -> Environment -> Environment
updateReturnFlag newFlag env@Env{ returnFlag = _ } = env { returnFlag = newFlag }

type Interpreter = StateT Environment IO

instance IStatement Interpreter where
  iIf :: Interpreter Bool -> Interpreter () 
         -> Interpreter ()
  iIf expr thn = do
    cond <- expr
    if cond
    then thn
    else return ()

  iIfElse :: Interpreter Bool -> Interpreter () -> Interpreter () 
             -> Interpreter ()
  iIfElse expr thn els = do
    cond <- expr
    if cond
    then thn
    else els

  iWhile :: Interpreter Bool -> Interpreter () 
            -> Interpreter ()
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
  
  iBreak :: Interpreter () 
  iBreak = modify $ updateBreakFlag True

  iContinue :: Interpreter ()
  iContinue = modify $ updateContinueFlag True
  
  iDefFunc0 :: String -> Interpreter () -> Interpreter ()
  iDefFunc0 name a = 
    modify $ updateFuncsMap (Map.insert name ([], a))
  iDefFunc1 :: String -> String -> Interpreter () -> Interpreter ()
  iDefFunc1 name arg1 a = 
    modify $ updateFuncsMap (Map.insert name ([arg1], a))
  iDefFunc2 :: String -> String -> String -> Interpreter () -> Interpreter () 
  iDefFunc2 name arg1 arg2 a = 
    modify $ updateFuncsMap (Map.insert name ([arg1, arg2], a))

  iReturn :: IPyType t => Interpreter t  -> Interpreter () 
  iReturn expr = do
    iAssign "~funcResult~" expr
    modify $ updateReturnFlag True
        
  
  iAssign    :: IPyType t => String -> Interpreter t  -> Interpreter ()
  iAssign name expr = do
    v <- expr
    ref <- lift $ newIORef $ PyType v
    modify $ updateRefMap (Map.insert name ref)
  
  iProcedure :: IPyType t => Interpreter t  -> Interpreter ()
  iProcedure expr = expr >> return ()

  iPrint :: IPyType t => Interpreter t  -> Interpreter ()
  iPrint a = do
    v <- a
    liftIO $ putStrLn $ show v

  iNextStmt :: Interpreter () -> Interpreter () -> Interpreter ()
  iNextStmt a b = do
    a
    env <- get
    if loopFlag env && (breakFlag env || continueFlag env)
    then return ()
    else b

instance IExpr Interpreter where
  iOr  :: Interpreter Bool -> Interpreter Bool -> Interpreter Bool
  iOr  a b = (||) <$> a <*> b
  iAnd :: Interpreter Bool -> Interpreter Bool -> Interpreter Bool
  iAnd a b = (&&) <$> a <*> b
  iNot :: Interpreter Bool -> Interpreter Bool
  iNot a   =  not <$> a

  iEq  :: IPyType t => Interpreter t -> Interpreter t -> Interpreter Bool
  iEq  a b = (==) <$> a <*> b
  iNEq :: IPyType t => Interpreter t -> Interpreter t -> Interpreter Bool
  iNEq a b = (/=) <$> a <*> b
  iLT  :: IPyType t => Interpreter t -> Interpreter t -> Interpreter Bool
  iLT  a b = (<)  <$> a <*> b
  iGT  :: IPyType t => Interpreter t -> Interpreter t -> Interpreter Bool
  iGT  a b = (>)  <$> a <*> b
  iLTE :: IPyType t => Interpreter t -> Interpreter t -> Interpreter Bool
  iLTE a b = (<=) <$> a <*> b
  iGTE :: IPyType t => Interpreter t -> Interpreter t -> Interpreter Bool
  iGTE a b = (>=) <$> a <*> b

  iBitOr  :: Interpreter Integer -> Interpreter Integer -> Interpreter Integer
  iBitOr  a b = (.|.) <$> a <*> b
  iBitXor :: Interpreter Integer -> Interpreter Integer -> Interpreter Integer
  iBitXor a b = (xor) <$> a <*> b
  iBitAnd :: Interpreter Integer -> Interpreter Integer -> Interpreter Integer
  iBitAnd a b = (.&.) <$> a <*> b
  iLeftShift  :: Interpreter Integer -> Interpreter Integer -> Interpreter Integer
  iLeftShift  a b = shift <$> a <*> (fromIntegral <$> b)
  iRightShift :: Interpreter Integer -> Interpreter Integer -> Interpreter Integer
  iRightShift a b = shift <$> a <*> (fromIntegral <$> iUnarMinus b)

  iPlus :: IPyNumType t => Interpreter t -> Interpreter t -> Interpreter t
  iPlus     a b = (+) <$> a <*> b
  iMinus :: IPyNumType t => Interpreter t -> Interpreter t -> Interpreter t
  iMinus    a b = (-) <$> a <*> b
  iMult :: IPyNumType t => Interpreter t -> Interpreter t -> Interpreter t
  iMult     a b = (*) <$> a <*> b
  iFloatDiv :: Interpreter Double -> Interpreter Double -> Interpreter Double
  iFloatDiv a b = (/) <$> iCastFloat a <*> iCastFloat b
  iDiv :: Interpreter Integer -> Interpreter Integer -> Interpreter Integer
  iDiv      a b = div <$> a <*> b
  iMod :: Interpreter Integer -> Interpreter Integer -> Interpreter Integer
  iMod      a b = mod <$> a <*> b
  iPow :: Interpreter Double -> Interpreter Double -> Interpreter Double
  iPow      a b = (**) <$> iCastFloat a <*> iCastFloat b
  iUnarPlus :: IPyNumType t => Interpreter t -> Interpreter t
  iUnarPlus = id
  iUnarMinus :: IPyNumType t => Interpreter t -> Interpreter t
  iUnarMinus a = (0 -) <$> a

  iStrPlus :: Interpreter String -> Interpreter String 
              -> Interpreter String
  iStrPlus a b = (++) <$> a <*> b

  iSlice1 :: Interpreter String 
             -> Interpreter Integer 
             -> Interpreter String
  iSlice1 s a = iSlice2 s a $ (+1) <$> a

  iSlice2 :: Interpreter String 
             -> Interpreter Integer -> Interpreter Integer 
             -> Interpreter String
  iSlice2 s a b = substr <$> a <*> b <*> s

  iCallFunc0 :: IPyType t => String -> Interpreter t
  iCallFunc0 name = do
    env <- get

    let (_, funcBlock) = funcs env Map.! name
    put initEnvironment
    modify $ updateFuncsMap (\_ -> funcs env)
    modify $ updateFuncFlag True

    funcBlock
    res <- iVariable "~funcResult~"

    put env
    return res
 
  iCallFunc1 :: IPyType t 
                => String -> Interpreter () 
                -> Interpreter t
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
    res <- iVariable "~funcResult~"

    put env
    return res

  
  iCallFunc2 :: IPyType t 
                => String -> Interpreter () -> Interpreter () 
                -> Interpreter t
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
    res <- iVariable "~funcResult~"
    
    put env
    return res

  iPushToStack :: IPyType t => Interpreter t -> Interpreter ()
  iPushToStack a = do
    v <- a
    ref <- lift $ newIORef $ PyType v
    modify $ pushStack ref

  iInput :: Interpreter String
  iInput = liftIO $ getLine

  iValue :: IPyType t => t -> Interpreter t
  iValue = return
  iVariable :: IPyType t => String -> Interpreter t
  iVariable name = do
    env <- get
    let ref = varRefs env Map.! name
    (PyType val) <- lift $ readIORef ref
    case cast val of 
      (Just j) -> return j
      Nothing -> error "iVariable: impossible case"

  iCastStr   :: IPyType t => Interpreter t -> Interpreter String
  iCastStr   a = castToStr     <$> a
  iCastInt   :: IPyType t => Interpreter t -> Interpreter Integer
  iCastInt   a = castToInteger <$> a
  iCastFloat :: IPyType t => Interpreter t -> Interpreter Double
  iCastFloat a = castToDouble  <$> a
  iCastBool  :: IPyType t => Interpreter t -> Interpreter Bool
  iCastBool  a = castToBool    <$> a
  iHidCastFloat :: IPyNumType t => Interpreter t -> Interpreter Double
  iHidCastFloat = iCastFloat

  iBrackets :: IPyType t => Interpreter t -> Interpreter t
  iBrackets = id

instance IPyScript Interpreter

substr :: Integer -> Integer -> String -> String
substr a b = take (fromIntegral $ b - a) . drop (fromIntegral a)

-- | Function gets python code as tagless final eDSL
-- and evaluates it.
interpret :: Interpreter () -> IO ()
interpret pyscript = evalStateT pyscript initEnvironment

-- | Function gets python code as string,
-- converts it to tagless final eDSL and evaluates it.
-- It uses Retyper's tfParse and interpret.
parseAndInterpret :: String -> IO ()
parseAndInterpret string =
  case runExcept $ tfParse string of
    Left err -> putStrLn $ show err
    Right pyscript -> interpret pyscript

-- | Function gets python code as tagless final eDSL,
-- evaluates it and returns all variables from environment.
getVariablesFromEnvironment :: Interpreter () -> IO (Map.Map String (IORef PyType))
getVariablesFromEnvironment pyscript = varRefs <$> execStateT pyscript initEnvironment

-- | Function gets python code as string, converts it to tagless final eDSL,
-- evaluates it and returns all variables from environment.
-- It uses Retyper's tfParse and getVariablesFromEnvironment.
parseAndGetVariables :: String -> IO (Map.Map String (IORef PyType))
parseAndGetVariables string =
  case runExcept $ tfParse string of
    Left err -> putStrLn (show err) >> return Map.empty
    Right pyscript -> getVariablesFromEnvironment pyscript