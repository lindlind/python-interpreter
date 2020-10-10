{
module Parser where

import Lexer
}

%name parseFile
%error {parseError}
%lexer {lexer} {TEof}
%monad {Alex}
%tokentype {Token}
%token
  ','    { TComma            _ }
  '.'    { TDot              _ }
  '='    { TAssign           _ }
  '('    { TOpenBracket      _ }
  ')'    { TCloseBracket     _ }
  '['    { TOpenSqrBracket   _ }
  ']'    { TCloseSqrBracket  _ }
  NUMBER { TNumber           _ value }
  VAR    { TVariable         _ name }

%%

Code
  :                       { [] }
  | Statements            { $1 }

Statements
  : Statement             { [$1] }

Statement
  : VAR '=' RHS           { Assign (name $1) $3 }

RHS
  : VAR '(' ')'           { Function (name $1) [] }
  | VAR '(' RHS ')'       { Function (name $1) [$3] }

{

lexer = (alexMonadScan >>=)

data Statement = Assign String Expression deriving (Eq, Show)

data Expression = Function String [Expression] deriving (Eq, Show)

parseError :: Token -> Alex a
parseError token = 
  case position token of
   (AlexPn _ line column) -> alexError $ "parse error at line " ++ (show line) ++ ", column " ++ (show column)

-- parse :: String -> Either String [Section]
parse s = runAlex s parseFile

}