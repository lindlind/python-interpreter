{
module Lexer where

import Data.List.Split
}

%wrapper "monad"
-- %wrapper "monadUserState"

$digit = [0-9]
$alpha = [A-Za-z]
$newline = \n

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

tokens :-
<0>                     $newline                        { makeToken LNewline }
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
<0>                     "or"                            { makeToken LOr }
<0>                     "and"                           { makeToken LAnd }
<0>                     "not"                           { makeToken LNot }
<0>                     "<"                             { makeToken LLT }
<0>                     ">"                             { makeToken LGT }
<0>                     "=="                            { makeToken LEq }
<0>                     "!="                            { makeToken LNEq }
<0>                     "<="                            { makeToken LLTE }
<0>                     ">="                            { makeToken LGTE }
<0>                     "|"                             { makeToken LBitOr }
<0>                     "^"                             { makeToken LBitXor }
<0>                     "&"                             { makeToken LBitAnd }
<0>                     "<<"                            { makeToken LLeftShift }
<0>                     ">>"                            { makeToken LRightShift }
<0>                     "+"                             { makeToken LPlus }
<0>                     "-"                             { makeToken LMinus }
<0>                     "*"                             { makeToken LMult }
<0>                     "//"                            { makeToken LDiv }
<0>                     "%"                             { makeToken LMod }
<0>                     "/"                             { makeToken LFloatDiv }
<0>                     @variable                       { makeToken LVariable }

{
data Lexeme 
  = LNewline
  | LType
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
  | LOr
  | LAnd
  | LNot
  | LLT
  | LGT
  | LEq
  | LNEq
  | LLTE
  | LGTE
  | LBitOr
  | LBitXor
  | LBitAnd
  | LLeftShift
  | LRightShift
  | LPlus
  | LMinus
  | LMult
  | LDiv
  | LMod
  | LFloatDiv
  deriving (Eq, Show)

readFloat :: String -> Float
readFloat s = 
  case 
    splitOn "." s 
  of
    s : [] -> (read s) :: Float
    int : rest : [] -> 
      case 
        splitOneOf "eE" rest 
      of
        r : []       -> (read ("0" ++ int ++ "." ++ r ++ "0"        )) :: Float
        r : exp : [] -> (read ("0" ++ int ++ "." ++ r ++ "0e" ++ exp)) :: Float


makeToken :: Lexeme -> AlexInput -> Int -> Alex Token
makeToken lexeme (pos, _, _, str) len = 
  let token = take len str
  in case lexeme of
    LNewline ->         return (TNewline          token pos)
    LType ->            return (TType             token pos token)
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
    LOr ->              return (TOr               token pos)
    LAnd ->             return (TAnd              token pos)
    LNot ->             return (TNot              token pos)
    LLT ->              return (TLT               token pos)
    LGT ->              return (TGT               token pos)
    LEq ->              return (TEq               token pos)
    LNEq ->             return (TNEq              token pos)
    LLTE ->             return (TLTE              token pos)
    LGTE ->             return (TGTE              token pos)
    LBitOr ->           return (TBitOr            token pos)
    LBitXor ->          return (TBitXor           token pos)
    LBitAnd ->          return (TBitAnd           token pos)
    LLeftShift ->       return (TLeftShift        token pos)
    LRightShift ->      return (TRightShift       token pos)
    LPlus ->            return (TPlus             token pos)
    LMinus ->           return (TMinus            token pos)
    LMult ->            return (TMult             token pos)
    LDiv ->             return (TDiv              token pos)
    LMod ->             return (TMod              token pos)
    LFloatDiv ->        return (TFloatDiv         token pos)

alexEOF :: Alex Token
alexEOF = return TEof

data Token
  = TNewline         { content :: String, position :: AlexPosn }
  | TType            { content :: String, position :: AlexPosn, name :: String }
  | TBool            { content :: String, position :: AlexPosn, bValue :: Bool }
  | TString          { content :: String, position :: AlexPosn, sValue :: String }
  | TInteger         { content :: String, position :: AlexPosn, iValue :: Integer }
  | TFloat           { content :: String, position :: AlexPosn, fValue :: Float }
  | TVariable        { content :: String, position :: AlexPosn, name :: String }
  | TComma           { content :: String, position :: AlexPosn }
  | TDot             { content :: String, position :: AlexPosn }
  | TOpenBracket     { content :: String, position :: AlexPosn }
  | TCloseBracket    { content :: String, position :: AlexPosn }
  | TOpenSqrBracket  { content :: String, position :: AlexPosn }
  | TCloseSqrBracket { content :: String, position :: AlexPosn }
  | TAssign          { content :: String, position :: AlexPosn }
  | TOr              { content :: String, position :: AlexPosn }
  | TAnd             { content :: String, position :: AlexPosn }
  | TNot             { content :: String, position :: AlexPosn }
  | TLT              { content :: String, position :: AlexPosn }
  | TGT              { content :: String, position :: AlexPosn }
  | TEq              { content :: String, position :: AlexPosn }
  | TNEq             { content :: String, position :: AlexPosn }
  | TLTE             { content :: String, position :: AlexPosn }
  | TGTE             { content :: String, position :: AlexPosn }
  | TBitOr           { content :: String, position :: AlexPosn }
  | TBitXor          { content :: String, position :: AlexPosn }
  | TBitAnd          { content :: String, position :: AlexPosn }
  | TLeftShift       { content :: String, position :: AlexPosn }
  | TRightShift      { content :: String, position :: AlexPosn }
  | TPlus            { content :: String, position :: AlexPosn }
  | TMinus           { content :: String, position :: AlexPosn }
  | TMult            { content :: String, position :: AlexPosn }
  | TDiv             { content :: String, position :: AlexPosn }
  | TMod             { content :: String, position :: AlexPosn }
  | TFloatDiv        { content :: String, position :: AlexPosn }
  | TEof
  deriving (Eq, Show)

}
