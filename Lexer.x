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
@multilineCommentContent = (\'\'\'.*\'\'\') | (\"\"\".*\"\"\")

@type = (int) | (float) | (bool) | (str)

@stringContent'  = [^\']*(\\[rnt\'\"\\][^\']*)*
@stringContent'' = [^\']*(\\[rnt\'\"\\][^\']*)*


@scientific = (\+|\-)? ( ([0-9]*\.?[0-9]+) | ([0-9]+\.) ) [eE](\+|\-)?[0-9]+
@boolean = (True) | (False)
@integer = (\+|\-)? ( 0 | ([1-9][0-9]*) )
@float = (\+|\-)? ( ([0-9]*\.[0-9]+) | ([0-9]+\.) )
@variable = $alpha [$alpha $digit \_]*

tokens :-
<0>                     $newline                        { makeToken LNewline }
<0>                     $white                          ;
<0>                     @onelineComment                 ;

<0>                     \'\'\'                          { begin mutlilineCommentSC' }
<0>                     \"\"\"                          { begin mutlilineCommentSC'' }
<mutlilineCommentSC'>   @multilineCommentContent        ;
<mutlilineCommentSC''>  @multilineCommentContent        ;
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
    LNewline ->         return (TNewline         pos)
    LType ->            return (TType            pos token)
    LBool ->            return (TBool            pos (case token of
                                                        "True" -> True
                                                        "False" -> False) 
                                                      )
    LString ->          return (TString          pos (take (len - 2) $ drop 1 token) )
    LInteger ->         return (TInteger         pos ((read token) :: Integer) )
    LFloat ->           return (TFloat           pos (readFloat token) )
    LVariable ->        return (TVariable        pos token)
    LComma ->           return (TComma           pos)
    LDot ->             return (TDot             pos)
    LOpenBracket ->     return (TOpenBracket     pos)
    LCloseBracket ->    return (TCloseBracket    pos)
    LOpenSqrBracket ->  return (TOpenSqrBracket  pos)
    LCloseSqrBracket -> return (TCloseSqrBracket pos)
    LAssign ->          return (TAssign          pos)
    LOr ->              return (TOr              pos)
    LAnd ->             return (TAnd             pos)
    LNot ->             return (TNot             pos)
    LLT ->              return (TLT              pos)
    LGT ->              return (TGT              pos)
    LEq ->              return (TEq              pos)
    LNEq ->             return (TNEq             pos)
    LLTE ->             return (TLTE             pos)
    LGTE ->             return (TGTE             pos)
    LBitOr ->           return (TBitOr           pos)
    LBitXor ->          return (TBitXor          pos)
    LBitAnd ->          return (TBitAnd          pos)
    LLeftShift ->       return (TLeftShift       pos)
    LRightShift ->      return (TRightShift      pos)
    LPlus ->            return (TPlus            pos)
    LMinus ->           return (TMinus           pos)
    LMult ->            return (TMult            pos)
    LDiv ->             return (TDiv             pos)
    LMod ->             return (TMod             pos)
    LFloatDiv ->        return (TFloatDiv        pos)

alexEOF :: Alex Token
alexEOF = return TEof

data Token
  = TNewline         { position :: AlexPosn }
  | TType            { position :: AlexPosn, name :: String }
  | TBool            { position :: AlexPosn, bValue :: Bool }
  | TString          { position :: AlexPosn, sValue :: String }
  | TInteger         { position :: AlexPosn, iValue :: Integer }
  | TFloat           { position :: AlexPosn, fValue :: Float }
  | TVariable        { position :: AlexPosn, name :: String }
  | TComma           { position :: AlexPosn }
  | TDot             { position :: AlexPosn }
  | TOpenBracket     { position :: AlexPosn }
  | TCloseBracket    { position :: AlexPosn }
  | TOpenSqrBracket  { position :: AlexPosn }
  | TCloseSqrBracket { position :: AlexPosn }
  | TAssign          { position :: AlexPosn }
  | TOr              { position :: AlexPosn }
  | TAnd             { position :: AlexPosn }
  | TNot             { position :: AlexPosn }
  | TLT              { position :: AlexPosn }
  | TGT              { position :: AlexPosn }
  | TEq              { position :: AlexPosn }
  | TNEq             { position :: AlexPosn }
  | TLTE             { position :: AlexPosn }
  | TGTE             { position :: AlexPosn }
  | TBitOr           { position :: AlexPosn }
  | TBitXor          { position :: AlexPosn }
  | TBitAnd          { position :: AlexPosn }
  | TLeftShift       { position :: AlexPosn }
  | TRightShift      { position :: AlexPosn }
  | TPlus            { position :: AlexPosn }
  | TMinus           { position :: AlexPosn }
  | TMult            { position :: AlexPosn }
  | TDiv             { position :: AlexPosn }
  | TMod             { position :: AlexPosn }
  | TFloatDiv        { position :: AlexPosn }
  | TEof
  deriving (Eq, Show)

}
