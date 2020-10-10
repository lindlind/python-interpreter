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
  @integer                        { makeToken LInteger }
  @variable                       { makeToken LVariable }
  ","                             { makeToken LComma }
  "."                             { makeToken LDot }
  "("                             { makeToken LOpenBracket }
  ")"                             { makeToken LCloseBracket }
  "["                             { makeToken LOpenSqrBracket }
  "]"                             { makeToken LCloseSqrBracket }
  "="                             { makeToken LAssign }
  "+"                             { makeToken LPlus }
  "-"                             { makeToken LMinus }
  "*"                             { makeToken LMult }
  "//"                            { makeToken LDiv }
  "%"                             { makeToken LMod }
  "/"                             { makeToken LFloatDiv }

{
data Lexeme 
  = LNewline
  | LType
  | LInteger
  | LVariable
  | LComma
  | LDot
  | LOpenBracket
  | LCloseBracket
  | LOpenSqrBracket
  | LCloseSqrBracket
  | LAssign
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
    LInteger ->         return (TInteger         pos ((read token) :: Integer) )
    LVariable ->        return (TVariable        pos token)
    LComma ->           return (TComma           pos)
    LDot ->             return (TDot             pos)
    LOpenBracket ->     return (TOpenBracket     pos)
    LCloseBracket ->    return (TCloseBracket    pos)
    LOpenSqrBracket ->  return (TOpenSqrBracket  pos)
    LCloseSqrBracket -> return (TCloseSqrBracket pos)
    LAssign ->          return (TAssign          pos)
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
  | TInteger         { position :: AlexPosn, value :: Integer }
  | TVariable        { position :: AlexPosn, name :: String }
  | TComma           { position :: AlexPosn }
  | TDot             { position :: AlexPosn }
  | TOpenBracket     { position :: AlexPosn }
  | TCloseBracket    { position :: AlexPosn }
  | TOpenSqrBracket  { position :: AlexPosn }
  | TCloseSqrBracket { position :: AlexPosn }
  | TAssign          { position :: AlexPosn }
  | TPlus            { position :: AlexPosn }
  | TMinus           { position :: AlexPosn }
  | TMult            { position :: AlexPosn }
  | TDiv             { position :: AlexPosn }
  | TMod             { position :: AlexPosn }
  | TFloatDiv        { position :: AlexPosn }
  | TEof
  deriving (Eq, Show)

}
