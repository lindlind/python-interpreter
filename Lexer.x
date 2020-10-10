{
module Lexer where
}

%wrapper "monad"
-- %wrapper "monadUserState"

$digit = [0-9]
$alpha = [A-Za-z]

@string = (\"[^\"]*\") | (\'[^\']*\')

tokens :-
    $white                          ;
    "#".                            ;
    ","                             { makeToken LComma }
    "."                             { makeToken LDot }
    "="                             { makeToken LAssign }
    "("                             { makeToken LOpenBracket }
    ")"                             { makeToken LCloseBracket }
    "["                             { makeToken LOpenSqrBracket }
    "]"                             { makeToken LCloseSqrBracket }
    $digit+                         { makeToken LNumber }
    $alpha [$alpha $digit \_]*      { makeToken LVariable }

{
data Lexeme = LComma 
            | LDot 
            | LAssign 
            | LOpenBracket 
            | LCloseBracket 
            | LOpenSqrBracket
            | LCloseSqrBracket 
            | LNumber
            | LVariable 
  deriving (Eq, Show)

makeToken :: Lexeme -> AlexInput -> Int -> Alex Token
makeToken lexeme (pos, _, _, str) len = 
  let token = take len str
  in case lexeme of
    LComma ->           return (TComma           pos)
    LDot ->             return (TDot             pos)
    LAssign ->          return (TAssign          pos)
    LOpenBracket ->     return (TOpenBracket     pos)
    LCloseBracket ->    return (TCloseBracket    pos)
    LOpenSqrBracket ->  return (TOpenSqrBracket  pos)
    LCloseSqrBracket -> return (TCloseSqrBracket pos)
    LNumber ->          return (TNumber          pos ((read token) :: Integer) )
    LVariable ->        return (TVariable        pos $ take (length token - 2) (drop 1 token))


-- No idea why I have to write this myself. Documentation doesn't mention it.
alexEOF :: Alex Token
alexEOF = return Eof

data Token
  = TComma           { position :: AlexPosn }
  | TDot             { position :: AlexPosn }
  | TAssign          { position :: AlexPosn }
  | TOpenBracket     { position :: AlexPosn }
  | TCloseBracket    { position :: AlexPosn }
  | TOpenSqrBracket  { position :: AlexPosn }
  | TCloseSqrBracket { position :: AlexPosn }
  | TNumber          { position :: AlexPosn, value :: Integer }
  | TVariable        { position :: AlexPosn, name :: String }
  | Eof
  deriving (Eq, Show)

}