module Main where

import PrettyPrinter
  ( parseAndPrettyPrint
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
import System.IO
  ( IOMode (ReadMode)
  , openFile
  , hGetContents
  )

main :: IO ()
main = do
  hspec prettyPrinterSpec


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
  ]

simpleFiles :: [String]
simpleFiles = 
  [ "compStrings"
  , "priorsBitwises"
  , "priorsNums"
  , "strangeVariables"
  , "swaps"
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
    ++ "expected = (1 << 5) - 2\n"
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
    ++ "a = 1.2345\n"
    ++ "b = 1234.5\n"
    ++ "c = True\n"
    ++ "tmp = c\n"
    ++ "c = a / b\n"
    ++ "a = int(tmp) - a\n"
    ++ "b = int(tmp) - b"
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