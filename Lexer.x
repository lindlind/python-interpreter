{
module Lexer where
}

%wrapper "monad"
-- %wrapper "monadUserState"

$digit = [0-9]
$alpha = [A-Za-z]
$newline = \n

@onelineComment = "#".*$newline
@multilineComment = (\'\'\'.*\'\'\') | (\"\"\".*\"\"\")

@type = (int) | (float) | (bool) | (str)

@string = (\"[^\"]*\") | (\'[^\']*\')
@boolean = (True) | (False)
@integer = (\+|\-)?0|([1-9][0-9]*)
@float = (\+|\-)?([0-9]*\.?[0-9]+)|([0-9]+\.)([eE](\+|\-)?[0-9]+)?

@variable = $alpha [$alpha $digit \_]*

tokens :-
  $newline                        { makeToken LNewline }
  $white                          ;
  @onelineComment                 ;
  @multilineComment               ;
  @type                           { makeToken LType }
  @boolean                        { makeToken LBool }
  @string                         { makeToken LString }
  @integer                        { makeToken LInteger }
  @float                          { makeToken LFloat }
  ","                             { makeToken LComma }
  "."                             { makeToken LDot }
  "("                             { makeToken LOpenBracket }
  ")"                             { makeToken LCloseBracket }
  "["                             { makeToken LOpenSqrBracket }
  "]"                             { makeToken LCloseSqrBracket }
  "="                             { makeToken LAssign }
  "or"                            { makeToken LOr }
  "and"                           { makeToken LAnd }
  "not"                           { makeToken LNot }
  "<"                             { makeToken LLT }
  ">"                             { makeToken LGT }
  "=="                            { makeToken LEq }
  "!="                            { makeToken LNEq }
  "<="                            { makeToken LLTE }
  ">="                            { makeToken LGTE }
  "|"                             { makeToken LBitOr }
  "^"                             { makeToken LBitXor }
  "&"                             { makeToken LBitAnd }
  "<<"                            { makeToken LLeftShift }
  ">>"                            { makeToken LRightShift }
  "+"                             { makeToken LPlus }
  "-"                             { makeToken LMinus }
  "*"                             { makeToken LMult }
  "//"                            { makeToken LDiv }
  "%"                             { makeToken LMod }
  "/"                             { makeToken LFloatDiv }
  @variable                       { makeToken LVariable }

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
    LFloat ->           return (TFloat           pos ((read token) :: Float) )
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
