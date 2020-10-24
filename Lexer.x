{
module Lexer where

import Data.List.Split
}

%wrapper "monadUserState"

$digit = [0-9]
$alpha = [A-Za-z]
$newline = \n

@spaces = (\ )+
@maybespaces = (\ )*
@onelineComment = "#".*$newline
@multilineCommentContent' = (.{1,2}|[^\']{3})*

@type = (int) | (float) | (bool) | (str)

@stringContent'  = [^\']*(\\[rnt\"\\][^\']*)*
@stringContent'' = [^\"]*(\\[rnt\'\\][^\"]*)*

@scientific = ( ([0-9]*\.?[0-9]+) | ([0-9]+\.) ) [eE](\+|\-)?[0-9]+
@boolean = (True) | (False)
@integer = ( 0 | ([1-9][0-9]*) )
@float = ( ([0-9]*\.[0-9]+) | ([0-9]+\.) )
@variable = $alpha [$alpha $digit \_]*

@comp = "=="|"!="|"<="|">="|"<"|">"

tokens :-
<0>                     $newline                        { makeToken LNewline `andBegin` startLine}
<startLine>            ^$white*\n                       ;
<0>                    ^@spaces                         { makeToken LTab }
<startLine>            ^@maybespaces                    { makeToken LTab  `andBegin` 0}
<0>                     $white                          ;

<0>                     "#"                             { begin onelineCommentSC }
<onelineCommentSC>      .                               ;
<onelineCommentSC>      $newline                        { (makeToken LNewline) `andBegin` 0 }

<0>                     \'\'\'                          { begin mutlilineCommentSC' }
<0>                     \"\"\"                          { begin mutlilineCommentSC'' }
<mutlilineCommentSC'>   [.\n]                           ;
<mutlilineCommentSC''>  [.\n]                           ;
<mutlilineCommentSC'>   \'\'\'                          { begin 0 }
<mutlilineCommentSC''>  \"\"\"                          { begin 0 }

<0>                     @type                           { makeToken LType }
<0>                     "input"                         { makeToken LInput }
<0>                     "print"                         { makeToken LPrint }
<0>                     "while"                         { makeToken LWhile }
<0>                     "if"                            { makeToken LIf }
<0>                     "elif"                          { makeToken LElif }
<0>                     "else"                          { makeToken LElse }
<0>                     "def"                           { makeToken LDef }
<0>                     "return"                        { makeToken LReturn }
<0>                     "break"                         { makeToken LBreak }
<0>                     "continue"                      { makeToken LContinue }
<0>                     @boolean                        { makeToken LBool }

<0>                     \'                              { begin stringSC' }
<0>                     \"                              { begin stringSC'' }
<stringSC'>             @stringContent'                 { makeToken LString }
<stringSC''>            @stringContent''                { makeToken LString }
<stringSC'>             \'                              { begin 0 }
<stringSC''>            \"                              { begin 0 }

<0>                     @scientific                     { makeToken LFloat }
<0>                     @integer                        { makeToken LInteger }
<0>                     @float                          { makeToken LFloat }
<0>                     ","                             { makeToken LComma }
<0>                     "("                             { makeToken LOpenBracket }
<0>                     ")"                             { makeToken LCloseBracket }
<0>                     "["                             { makeToken LOpenSqrBracket }
<0>                     "]"                             { makeToken LCloseSqrBracket }
<0>                     "="                             { makeToken LAssign }
<0>                     "->"                            { makeToken LArrow }
<0>                     "or"                            { makeToken LOr }
<0>                     "and"                           { makeToken LAnd }
<0>                     "not"                           { makeToken LNot }
<0>                     @comp                           { makeToken LComp }
<0>                     "|"                             { makeToken LBitOr }
<0>                     "^"                             { makeToken LBitXor }
<0>                     "&"                             { makeToken LBitAnd }
<0>                     "<<"                            { makeToken LLeftShift }
<0>                     ">>"                            { makeToken LRightShift }
<0>                     "+"                             { makeToken LPlus }
<0>                     "-"                             { makeToken LMinus }
<0>                     "**"                            { makeToken LPower }
<0>                     "*"                             { makeToken LMult }
<0>                     "//"                            { makeToken LDiv }
<0>                     "%"                             { makeToken LMod }
<0>                     "/"                             { makeToken LFloatDiv }
<0>                     ":"                             { makeToken LSlice }
<0>                     @variable                       { makeToken LVariable }

{

data Lexeme
  = LNewline
  | LTab
  | LType
  | LInput
  | LPrint
  | LWhile
  | LIf
  | LElif
  | LElse
  | LDef
  | LReturn
  | LBreak
  | LContinue
  | LBool
  | LString
  | LInteger
  | LFloat
  | LVariable
  | LComma
  | LDot
  | LOpenBracket
  | LCloseBracket
  | LOpenSqrBracket
  | LCloseSqrBracket
  | LAssign
  | LArrow
  | LOr
  | LAnd
  | LNot
  | LComp
  | LBitOr
  | LBitXor
  | LBitAnd
  | LLeftShift
  | LRightShift
  | LPlus
  | LMinus
  | LPower
  | LMult
  | LDiv
  | LMod
  | LFloatDiv
  | LSlice
  deriving (Eq, Show)

readFloat :: String -> Double
readFloat s =
  case
    splitOn "." s
  of
    s : [] -> (read s) :: Double
    int : rest : [] ->
      case
        splitOneOf "eE" rest
      of
        r : []       -> (read ("0" ++ int ++ "." ++ r ++ "0"        )) :: Double
        r : exp : [] -> (read ("0" ++ int ++ "." ++ r ++ "0e" ++ exp)) :: Double

lexIndentError :: AlexPosn -> String
lexIndentError (AlexPn _ line _) = "lexical indentation error at line " ++ (show line)

makeToken :: Lexeme -> AlexInput -> Int -> Alex Token
makeToken lexeme (pos, _, _, str) len =
  let
    token = take len str
    (AlexPn _ line column) = pos
  in case lexeme of
    LTab ->
      if len `mod` 4 == 0
        then do
          let curIndent = len `div` 4
          prevIndent <- getPrevIndent
          setPrevIndent curIndent
          case (curIndent - prevIndent) of
            ( 1) ->     return (TIndent      "<indent>" pos)
            ( 0) -> alexMonadScan
            (-1) ->     return (TDedent      "<dedent>" pos)
            _    -> alexError $ lexIndentError pos
        else alexError $ lexIndentError pos

    LNewline ->         return (TNewline          token pos)
    LType ->            return (TType             token pos token)
    LInput ->           return (TInput            token pos)
    LPrint ->           return (TPrint            token pos)
    LWhile ->           return (TWhile            token pos)
    LIf ->              return (TIf               token pos)
    LElif ->            return (TElif             token pos)
    LElse ->            return (TElse             token pos)
    LDef ->             return (TDef              token pos)
    LReturn ->          return (TReturn           token pos)
    LBreak ->           return (TBreak            token pos)
    LContinue ->        return (TContinue         token pos)
    LBool ->            return (TBool             token pos $
                                                    case token of
                                                      "True" -> True
                                                      "False" -> False
                                                  )
    LString ->          return (TString           token pos token)
    LInteger ->         return (TInteger          token pos $
                                                    ((read token) :: Integer)
                                                  )
    LFloat ->           return (TFloat            token pos $
                                                    readFloat token
                                                  )
    LVariable ->        return (TVariable         token pos token)
    LComma ->           return (TComma            token pos)
    LDot ->             return (TDot              token pos)
    LOpenBracket ->     return (TOpenBracket      token pos)
    LCloseBracket ->    return (TCloseBracket     token pos)
    LOpenSqrBracket ->  return (TOpenSqrBracket   token pos)
    LCloseSqrBracket -> return (TCloseSqrBracket  token pos)
    LAssign ->          return (TAssign           token pos)
    LArrow ->           return (TArrow            token pos)
    LOr ->              return (TOr               token pos)
    LAnd ->             return (TAnd              token pos)
    LNot ->             return (TNot              token pos)
    LComp ->            return (TComp             token pos)
    LBitOr ->           return (TBitOr            token pos)
    LBitXor ->          return (TBitXor           token pos)
    LBitAnd ->          return (TBitAnd           token pos)
    LLeftShift ->       return (TLeftShift        token pos)
    LRightShift ->      return (TRightShift       token pos)
    LPlus ->            return (TPlus             token pos)
    LMinus ->           return (TMinus            token pos)
    LPower ->           return (TPower            token pos)
    LMult ->            return (TMult             token pos)
    LDiv ->             return (TDiv              token pos)
    LMod ->             return (TMod              token pos)
    LFloatDiv ->        return (TFloatDiv         token pos)
    LSlice ->           return (TSlice            token pos)

data Token
  = TNewline         { content :: String, position :: AlexPosn }
  | TIndent          { content :: String, position :: AlexPosn }
  | TDedent          { content :: String, position :: AlexPosn }
  | TType            { content :: String, position :: AlexPosn, name :: String }
  | TInput           { content :: String, position :: AlexPosn }
  | TPrint           { content :: String, position :: AlexPosn }
  | TWhile           { content :: String, position :: AlexPosn }
  | TIf              { content :: String, position :: AlexPosn }
  | TElif            { content :: String, position :: AlexPosn }
  | TElse            { content :: String, position :: AlexPosn }
  | TDef             { content :: String, position :: AlexPosn }
  | TReturn          { content :: String, position :: AlexPosn }
  | TBreak           { content :: String, position :: AlexPosn }
  | TContinue        { content :: String, position :: AlexPosn }
  | TBool            { content :: String, position :: AlexPosn, bVal :: Bool }
  | TInteger         { content :: String, position :: AlexPosn, iVal :: Integer }
  | TFloat           { content :: String, position :: AlexPosn, fVal :: Double }
  | TString          { content :: String, position :: AlexPosn, sVal :: String }
  | TVariable        { content :: String, position :: AlexPosn, name :: String }
  | TComma           { content :: String, position :: AlexPosn }
  | TDot             { content :: String, position :: AlexPosn }
  | TOpenBracket     { content :: String, position :: AlexPosn }
  | TCloseBracket    { content :: String, position :: AlexPosn }
  | TOpenSqrBracket  { content :: String, position :: AlexPosn }
  | TCloseSqrBracket { content :: String, position :: AlexPosn }
  | TAssign          { content :: String, position :: AlexPosn }
  | TArrow           { content :: String, position :: AlexPosn }
  | TOr              { content :: String, position :: AlexPosn }
  | TAnd             { content :: String, position :: AlexPosn }
  | TNot             { content :: String, position :: AlexPosn }
  | TComp            { content :: String, position :: AlexPosn }
  | TBitOr           { content :: String, position :: AlexPosn }
  | TBitXor          { content :: String, position :: AlexPosn }
  | TBitAnd          { content :: String, position :: AlexPosn }
  | TLeftShift       { content :: String, position :: AlexPosn }
  | TRightShift      { content :: String, position :: AlexPosn }
  | TPlus            { content :: String, position :: AlexPosn }
  | TMinus           { content :: String, position :: AlexPosn }
  | TPower           { content :: String, position :: AlexPosn }
  | TMult            { content :: String, position :: AlexPosn }
  | TDiv             { content :: String, position :: AlexPosn }
  | TMod             { content :: String, position :: AlexPosn }
  | TFloatDiv        { content :: String, position :: AlexPosn }
  | TSlice           { content :: String, position :: AlexPosn }
  | TEof
  deriving (Eq, Show)

-- AlexUserState monadic requirements

alexEOF :: Alex Token
alexEOF = return TEof

data AlexUserState = AlexUserState { prevIndent :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { prevIndent = 0 }

getPrevIndent :: Alex Int
getPrevIndent = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, prevIndent ust)

setPrevIndent :: Int -> Alex ()
setPrevIndent indent = Alex $ \s -> Right (s{alex_ust=(alex_ust s){prevIndent=indent}}, ())

}
