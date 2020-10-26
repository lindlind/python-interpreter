{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE GADTs    #-}

module Main where

import ClassDef
  ( PyType (..)
  )
import Interpreter
  ( parseAndGetVariables
  )
import PrettyPrinter
  ( parseAndPrettyPrint
  )

import Data.IORef
  ( IORef
  , readIORef
  )
import qualified Data.Map.Strict as Map
  ( Map
  , fromList
  , toAscList
  )
import System.IO
  ( IOMode (ReadMode)
  , openFile
  , hGetContents
  )
import Test.Hspec 
  ( Arg
  , Expectation
  , Spec
  , SpecWith
  , context
  , describe
  , hspec
  , it
  , runIO
  , shouldBe
  )
import Type.Reflection
  ( (:~~:) (HRefl)
  , eqTypeRep
  , typeRep
  )

main :: IO ()
main = do
  hspec prettyPrinterSpec
  hspec interpreterSpec


readAllFiles :: String -> [String] -> IO [String]
readAllFiles _ [] = return []
readAllFiles fileDir (fileName : others) = do
  inh <- openFile (fileDir ++ fileName ++ ".py") ReadMode
  h <- hGetContents inh
  t <- readAllFiles fileDir others
  return $ h : t

triviaProcedureFiles :: [String]
triviaProcedureFiles = 
  [ "procStr"
  , "procInt"
  , "procFloat"
  , "procBool"
  , "procOr"
  , "procAnd"
  , "procNot"
  , "procEq"
  , "procNEq"
  , "procLT"
  , "procGT"
  , "procLTE"
  , "procGTE"
  , "procBitOr"
  , "procBitXor"
  , "procBitAnd"
  , "procLeftShift"
  , "procRightShift"
  , "procPlus"
  , "procMinus"
  , "procMult"
  , "procFloatDiv"
  , "procDiv"
  , "procMod"
  , "procPow"
  , "procUnarPlus"
  , "procUnarMinus"
  , "procStrPlus"
  , "procSlice1"
  , "procSlice2"
  , "procCastStr"
  , "procCastInt"
  , "procCastFloat"
  , "procCastBool"
  , "procBrackets"
  ]

triviaAssignFiles :: [String]
triviaAssignFiles = 
  [ "assignStr"
  , "assignInt"
  , "assignFloat"
  , "assignBool"
  , "assignOr"
  , "assignAnd"
  , "assignNot"
  , "assignEq"
  , "assignNEq"
  , "assignLT"
  , "assignGT"
  , "assignLTE"
  , "assignGTE"
  , "assignBitOr"
  , "assignBitXor"
  , "assignBitAnd"
  , "assignLeftShift"
  , "assignRightShift"
  , "assignPlus"
  , "assignMinus"
  , "assignMult"
  , "assignFloatDiv"
  , "assignDiv"
  , "assignMod"
  , "assignPow"
  , "assignUnarPlus"
  , "assignUnarMinus"
  , "assignStrPlus"
  , "assignSlice1"
  , "assignSlice2"
  , "assignCastStr"
  , "assignCastInt"
  , "assignCastFloat"
  , "assignCastBool"
  , "assignBrackets"
  , "reAssign"
  ]

simpleFiles :: [String]
simpleFiles = 
  [ "compStrings"
  , "priorsBitwises"
  , "priorsNums"
  , "strangeVariables"
  , "swaps"
  ]

compoundFiles :: [String]
compoundFiles = 
  [ "if"
  , "ifElse"
  , "elif"
  , "while"
  , "func"
  ]

negativeFiles :: [String]
negativeFiles = 
  [ "empty"
  , "common"
  , "lastLine"
  , "type"
  , "opType"
  , "notDefVar"
  , "notDefFunc"
  , "redefFunc"
  , "wrongArgs"
  , "wrongCntArgs"
  ]

triviaProcedurePrinterResults :: [String]
triviaProcedurePrinterResults = 
  [ "\"string\"\n\"\""
  , "1"
  , "1.0"
  , "True"
  , "True or False"
  , "True and False"
  , "not True"
  , "\"abc\" == \"abc\"\n42 == 42\n13.37 == 13.37\nTrue == False"
  , "\"abc\" != \"abc\"\n42 != 42\n13.37 != 13.37\nTrue != False"
  , "\"abc\" < \"abc\"\n42 < 42\n13.37 < 13.37\nTrue < False"
  , "\"abc\" > \"abc\"\n42 > 42\n13.37 > 13.37\nTrue > False"
  , "\"abc\" <= \"abc\"\n42 <= 42\n13.37 <= 13.37\nTrue <= False"
  , "\"abc\" >= \"abc\"\n42 >= 42\n13.37 >= 13.37\nTrue >= False"
  , "14 | 15"
  , "14 ^ 15"
  , "14 & 15"
  , "14 << 2"
  , "14 >> 2"
  , "42 + 42\n13.37 + 13.37"
  , "42 - 42\n13.37 - 13.37"
  , "42 * 42\n13.37 * 13.37"
  , "5 / 2\n5.0 / 1000.0"
  , "5 // 2"
  , "5 % 2"
  , "5**2\n5.0**3.0"
  , "+++++++++++42\n+++1.0e-4"
  , "-----------42\n-113.37"
  , "\"22\" + \"halp me please\""
  , "\"abacaba\"[1]"
  , "\"abacaba\"[1:5]"
  , "str(\"HALP\")\nstr(42)\nstr(13.37)\nstr(True)"
  , "int(\"88005553535\")\nint(42)\nint(13.37)\nint(True)"
  , "float(\"8800.55535e35\")\nfloat(42)\nfloat(13.37)\nfloat(True)"
  , "bool(\"False\")\nbool(0)\nbool(13.37)\nbool(True)"
  , "((((\"brackets\"))))"
  ]

triviaAssignPrinterResults :: [String]
triviaAssignPrinterResults = 
  [ "a = \"string\"\nb = \"\""
  , "a = 1"
  , "a = 1.2"
  , "a = True"
  , "a = True or False"
  , "a = True and False"
  , "a = not True"
  ,    "a = \"abc\" == \"abc\"\n"
    ++ "b = 42 == 42\n"
    ++ "c = 13.37 == 13.37\n"
    ++ "d = True == False"
  ,    "a = \"abc\" != \"abc\"\n"
    ++ "b = 42 != 42\n"
    ++ "c = 13.37 != 13.37\n"
    ++ "d = True != False"
  ,    "a = \"abc\" < \"abc\"\n"
    ++ "b = 42 < 42\n"
    ++ "c = 13.37 < 13.37\n"
    ++ "d = True < False"
  ,    "a = \"abc\" > \"abc\"\n"
    ++ "b = 42 > 42\n"
    ++ "c = 13.37 > 13.37\n"
    ++ "d = True > False"
  ,    "a = \"abc\" <= \"abc\"\n"
    ++ "b = 42 <= 42\n"
    ++ "c = 13.37 <= 13.37\n"
    ++ "d = True <= False"
  ,    "a = \"abc\" >= \"abc\"\n"
    ++ "b = 42 >= 42\n"
    ++ "c = 13.37 >= 13.37\n"
    ++ "d = True >= False"
  , "a = 14 | 15"
  , "a = 14 ^ 15"
  , "a = 14 & 15"
  , "a = 14 << 2"
  , "a = 14 >> 2"
  , "a = 42 + 42\nb = 13.37 + 13.37"
  , "a = 42 - 42\nb = 13.37 - 13.37"
  , "a = 42 * 42\nb = 13.37 * 13.37"
  , "a = 14 / 4\nb = 14.2 / 15.1"
  , "a = 14 // 4"
  , "a = 14 % 4"
  , "a = 14**4\nb = 14.2**0.1"
  , "a = +42\nb = +0.12"
  , "a = --------------------42\nb = --1000.0"
  , "a = \"they took my passport\" + \"call the mr president please\""
  , "a = \"abacaba\"[1]"
  , "a = \"abacaba\"[1:5]"
  ,    "a = str(\"I will be free 2.11.2020\")\n"
    ++ "b = str(42)\n"
    ++ "c = str(13.37)\n"
    ++ "d = str(True)"
  ,    "s = int(\"88005553535\")\n"
    ++ "i = int(42)\n"
    ++ "f = int(13.37)\n"
    ++ "b = int(True)"
  ,    "s = float(\"8800.55535e35\")\n"
    ++ "i = float(42)\n"
    ++ "f = float(13.37)\n"
    ++ "b = float(True)"
  ,    "s = bool(\"False\")\n"
    ++ "i = bool(0)\n"
    ++ "f = bool(13.37)\n"
    ++ "b = bool(True)"
  , "b = ((((\"brackets\"))))"
  , "a = 1\na = \"2\""
  ]

simplePrinterResults :: [String]
simplePrinterResults =
  [    "smallString = \"xyz\"\n"
    ++ "longString = \"abacaba\"\n"
    ++ "spaces = \"   \"\n"
    ++ "byLen1 = smallString < longString\n"
    ++ "byLen2 = smallString + spaces == longString\n"
    ++ "byLex1 = smallString > longString\n"
    ++ "byLex2 = smallString + spaces != longString\n"
    ++ "emptyStringDoNothing1 = smallString == smallString + \"\" + \"\"\n"
    ++ "emptyStringDoNothing2 = longString == \"\" + \"\" + longString\n"
    ++ "emptyStringDoNothing3 = \"\" + spaces + \"\""

  ,    "a = 23 & 26 | 10 ^ 15\n"
    ++ "expected = (1 << 5) - 1 - (1 << 3)\n"
    ++ "true = a == expected"

  ,    "a = 2 + 2 * 2\n"
    ++ "b = 2 + (2 * 2)\n"
    ++ "c = (2 + 2) * 2\n"
    ++ "true = a == b and a != c"

  ,    "a = 1\n"
    ++ "A = 2\n"
    ++ "aAaAaA = 3\n"
    ++ "seroke11 = 4\n"
    ++ "bla_bla_bla = 5\n"
    ++ "d__________________b = 6\n"
    ++ "p12345_67890q_ = 7"

  ,    "a = 5\n"
    ++ "b = \"q\"\n"
    ++ "tmp = a\n"
    ++ "a = b\n"
    ++ "b = tmp\n"
    ++ "a = 1.2339\n"
    ++ "b = 1233.9\n"
    ++ "c = True\n"
    ++ "tmp = c\n"
    ++ "c = a / b\n"
    ++ "a = int(tmp) - a\n"
    ++ "b = int(tmp) - b"
  ]

compoundPrinterResults :: [String]
compoundPrinterResults = 
  [    "a = 33\n"
    ++ "b = 3\n"
    ++ "if a % b == 0:\n"
    ++ "    c = a // b\n"
    ++ "x = 31\n"
    ++ "y = 3\n"
    ++ "if x % y == 0:\n"
    ++ "    z = x // y"

  ,    "a = 33\n"
    ++ "b = 3\n"
    ++ "if a % b == 0:\n"
    ++ "    c = a // b\n"
    ++ "else:\n"
    ++ "    c = a % b\n"
    ++ "x = 31\n"
    ++ "y = 3\n"
    ++ "if x % y == 0:\n"
    ++ "    z = x // y\n"
    ++ "else:\n"
    ++ "    z = x % y"

  ,    "zero = 0\n"
    ++ "if zero < -1:\n"
    ++ "    verd = \"too negative\"\n"
    ++ "else:\n"
    ++ "    if zero > 1:\n"
    ++ "        verd = \"too positive\"\n"
    ++ "    else:\n"
    ++ "        if zero == 0:\n"
    ++ "            verd = \"that's it\"\n"
    ++ "        else:\n"
    ++ "            verd = \"it was close\"\n"
    ++ "verd == \"that's it\""

  , "s = \"abacaba\"\n"
    ++ "length = 1\n"
    ++ "while s[0:length] != s:\n"
    ++ "    length = length + 1\n"
    ++ "length == 7"

  ,    "def f0():\n"
    ++ "    return 42\n"
    ++ "def f1(x):\n"
    ++ "    return x ^ 42\n"
    ++ "def f2(x, y):\n"
    ++ "    q = int(y)\n"
    ++ "    return x == q\n"
    ++ "r0 = f0()\n"
    ++ "r1 = f1(42 >> 1) + 1\n"
    ++ "r2 = f2(56, \"56\")"
  ]

negativePrinterResults :: [String]
negativePrinterResults = 
  [ "Syntax error: empty file"
  , "lexical error at line 1, column 2"
  , "Unexpected end of file (last empty line may be missed)"
  , "TypeError: wrong type in if statement; line 1, column 1, possible types: bool"
  , "OperationTypeError: wrong type in operation '+'; line 1, column 7"
  , "VarNotDefinedError: wrong variable 'a'; line 1, column 5"
  , "FunctionNotDefinedError: wrong function 'f'; line 1, column 5"
  , "FunctionRedefinitionError: multiple definition of function 'f'"
  , "FunctionArgsTypeError: wrong type of first variable in function 'f'; line 4, column 1"
  , "FunctionArgsCountError: wrong number of arguments in function 'f'; line 4, column 1"
  ]

triviaAssignInterpreterResults :: [Map.Map String PyType]
triviaAssignInterpreterResults = map Map.fromList $ 
  [ [ ("a", PyType "string")
    , ("b", PyType "")
    ]
  , [ ("a", PyType (1 :: Integer)) ]
  , [ ("a", PyType (1.2 :: Double)) ]
  , [ ("a", PyType True) ]
  , [ ("a", PyType True) ]
  , [ ("a", PyType False) ]
  , [ ("a", PyType False) ]
  , [ ("a", PyType True)
    , ("b", PyType True)
    , ("c", PyType True)
    , ("d", PyType False)
    ]
  , [ ("a", PyType False)
    , ("b", PyType False)
    , ("c", PyType False)
    , ("d", PyType True)
    ]
  , [ ("a", PyType False)
    , ("b", PyType False)
    , ("c", PyType False)
    , ("d", PyType False)
    ]
  , [ ("a", PyType False)
    , ("b", PyType False)
    , ("c", PyType False)
    , ("d", PyType True)
    ]
  , [ ("a", PyType True)
    , ("b", PyType True)
    , ("c", PyType True)
    , ("d", PyType False)
    ]
  , [ ("a", PyType True)
    , ("b", PyType True)
    , ("c", PyType True)
    , ("d", PyType True)
    ]
  , [ ("a", PyType (15 :: Integer)) ]
  , [ ("a", PyType (1 :: Integer)) ]
  , [ ("a", PyType (14 :: Integer)) ]
  , [ ("a", PyType (56 :: Integer)) ]
  , [ ("a", PyType (3 :: Integer)) ]
  , [ ("a", PyType (84 :: Integer))
    , ("b", PyType (26.74 :: Double))
    ]
  , [ ("a", PyType (0 :: Integer))
    , ("b", PyType (0.0 :: Double))
    ]
  , [ ("a", PyType (1764 :: Integer))
    , ("b", PyType (13.37*13.37 :: Double))
    ]
  , [ ("a", PyType (3.5 :: Double))
    , ("b", PyType (14.2 / 15.1 :: Double))
    ]
  , [ ("a", PyType (3 :: Integer)) ]
  , [ ("a", PyType (2 :: Integer)) ]
  , [ ("a", PyType (38416.0:: Double))
    , ("b", PyType ((14.2**0.1) :: Double))
    ]
  , [ ("a", PyType (42 :: Integer))
    , ("b", PyType (0.12 :: Double))
    ]
  , [ ("a", PyType (42 :: Integer))
    , ("b", PyType (1000.0 :: Double))
    ]
  , [ ("a", PyType "they took my passportcall the mr president please") ]
  , [ ("a", PyType "b") ]
  , [ ("a", PyType "baca") ]
  , [ ("a", PyType "I will be free 2.11.2020")
    , ("b", PyType "42")
    , ("c", PyType "13.37")
    , ("d", PyType "True")
    ]
  , [ ("s", PyType (88005553535 :: Integer))
    , ("i", PyType (42 :: Integer))
    , ("f", PyType (13 :: Integer))
    , ("b", PyType (1 :: Integer))
    ]
  , [ ("s", PyType (8800.55535e35 :: Double))
    , ("i", PyType (42.0 :: Double))
    , ("f", PyType (13.37 :: Double))
    , ("b", PyType (1.0 :: Double))
    ]
  , [ ("s", PyType False)
    , ("i", PyType False)
    , ("f", PyType True)
    , ("b", PyType True)
    ]
  , [ ("b", PyType "brackets") ]
  , [ ("a", PyType "2") ]
  ]

simpleInterpreterResults :: [Map.Map String PyType]
simpleInterpreterResults = map Map.fromList $ 
  [ [ ("smallString", PyType "xyz")
    , ("longString", PyType "abacaba")
    , ("spaces", PyType "   ")
    , ("byLen1", PyType False)
    , ("byLen2", PyType False)
    , ("byLex1", PyType True)
    , ("byLex2", PyType True)
    , ("emptyStringDoNothing1", PyType True)
    , ("emptyStringDoNothing2", PyType True)
    , ("emptyStringDoNothing3", PyType "   ")
    ]
  , [ ("a", PyType (23 :: Integer))
    , ("expected", PyType (23 :: Integer))
    , ("true", PyType True)
    ]
  , [ ("a", PyType (6 :: Integer))
    , ("b", PyType (6 :: Integer))
    , ("c", PyType (8 :: Integer))
    , ("true", PyType True)
    ]
  , [ ("a", PyType (1 :: Integer))
    , ("A", PyType (2 :: Integer))
    , ("aAaAaA", PyType (3 :: Integer))
    , ("seroke11", PyType (4 :: Integer))
    , ("bla_bla_bla", PyType (5 :: Integer))
    , ("d__________________b", PyType (6 :: Integer))
    , ("p12345_67890q_", PyType (7 :: Integer))
    ]
  , [ ("tmp", PyType True)
    , ("c", PyType (1.2339 / 1233.9 :: Double))
    , ("a", PyType ((-0.2339) :: Double))
    , ("b", PyType ((-1232.9) :: Double))
    ]
  ]

compoundInterpreterResults :: [Map.Map String PyType]
compoundInterpreterResults = map Map.fromList $ 
  [ [ ("a", PyType (33 :: Integer))
    , ("b", PyType (3 :: Integer))
    , ("c", PyType (11 :: Integer))
    , ("x", PyType (31 :: Integer))
    , ("y", PyType (3 :: Integer))
    ]
  , [ ("a", PyType (33 :: Integer))
    , ("b", PyType (3 :: Integer))
    , ("c", PyType (11 :: Integer))
    , ("x", PyType (31 :: Integer))
    , ("y", PyType (3 :: Integer))
    , ("z", PyType (1 :: Integer))
    ]
  , [ ("zero", PyType (0 :: Integer))
    , ("verd", PyType "that's it")
    ]
  , [ ("length", PyType (7 :: Integer))
    , ("s", PyType "abacaba")
    ]
  , [ ("r0", PyType (42 :: Integer))
    , ("r1", PyType (64 :: Integer))
    , ("r2", PyType True)
    ]
  ]

checkPrettyPrinter :: [String] -> [String] -> [String] 
                      -> SpecWith (Arg Expectation)
checkPrettyPrinter (curFile : otherFiles) 
                   (curCase : otherCases) 
                   (curResult : otherResults) = do
  it curFile $ parseAndPrettyPrint curCase `shouldBe` curResult
  checkPrettyPrinter otherFiles otherCases otherResults
checkPrettyPrinter _ _ _ = return ()

prettyPrinterSpec :: Spec
prettyPrinterSpec = do
  describe "prettyPrinter" $ do
    context "trivia procedures" $ do
      triviaProcedureCases <- 
        runIO (readAllFiles "tests\\src\\trivia\\" triviaProcedureFiles)
      checkPrettyPrinter triviaProcedureFiles 
                         triviaProcedureCases 
                         triviaProcedurePrinterResults
    context "trivia assigns" $ do
      triviaAssignCases <- 
        runIO (readAllFiles "tests\\src\\trivia\\" triviaAssignFiles)
      checkPrettyPrinter triviaAssignFiles 
                         triviaAssignCases 
                         triviaAssignPrinterResults
    context "simple cases" $ do
      simpleCases <- 
        runIO (readAllFiles "tests\\src\\simple\\" simpleFiles)
      checkPrettyPrinter simpleFiles 
                         simpleCases 
                         simplePrinterResults
    context "compound cases" $ do
      compoundCases <- 
        runIO (readAllFiles "tests\\src\\compound\\" compoundFiles)
      checkPrettyPrinter compoundFiles 
                         compoundCases 
                         compoundPrinterResults

equalPyTypes :: PyType -> PyType -> Bool
equalPyTypes (PyType (x :: a)) (PyType (y :: b)) =
  case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> x == y
    Nothing -> False

equalPyTypeLists :: [IORef PyType] -> [PyType] -> IO Bool
equalPyTypeLists (ref : aOthers) (b : bOthers) = do 
  a <- readIORef ref
  okOthers <- aOthers `equalPyTypeLists` bOthers
  return $ (a `equalPyTypes` b) && okOthers
equalPyTypeLists [] [] = return True
equalPyTypeLists _ _ = return False

equalPyTypeMaps :: Map.Map String (IORef PyType) 
                  -> Map.Map String PyType
                  -> IO Bool
equalPyTypeMaps mpA mpB = 
  map snd (Map.toAscList mpA) `equalPyTypeLists` map snd (Map.toAscList mpB)

checkInterpreter :: [String] -> [String] -> [Map.Map String PyType]
                    -> SpecWith (Arg Expectation)
checkInterpreter (curFile : otherFiles) 
                 (curCase : otherCases) 
                 (curResult : otherResults) = do
  mp <- runIO (parseAndGetVariables curCase)
  mapsAreEqual <- runIO $ mp `equalPyTypeMaps` curResult
  it curFile $ mapsAreEqual `shouldBe` True
  checkInterpreter otherFiles otherCases otherResults
checkInterpreter _ _ _ = return ()

interpreterSpec :: Spec
interpreterSpec = do
  describe "interpreter" $ do
    context "trivia assigns" $ do
      triviaAssignCases <- 
        runIO (readAllFiles "tests\\src\\trivia\\" triviaAssignFiles)
      checkInterpreter triviaAssignFiles 
                       triviaAssignCases 
                       triviaAssignInterpreterResults
    context "simple cases" $ do
      simpleCases <- 
        runIO (readAllFiles "tests\\src\\simple\\" simpleFiles)
      checkInterpreter simpleFiles 
                       simpleCases 
                       simpleInterpreterResults
    context "compound cases" $ do
      compoundCases <- 
        runIO (readAllFiles "tests\\src\\compound\\" compoundFiles)
      checkInterpreter compoundFiles 
                       compoundCases 
                       compoundInterpreterResults
    context "invalid cases" $ do
      negativeCases <- 
        runIO (readAllFiles "tests\\src\\negative\\" negativeFiles)
      checkPrettyPrinter negativeFiles 
                         negativeCases 
                         negativePrinterResults