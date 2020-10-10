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
  NEWLINE       { TNewline          _ }
  TYPE          { TType             _ name }
  INT           { TInteger          _ value }
  VAR           { TVariable         _ name }
  ','           { TComma            _ }
  '.'           { TDot              _ }
  '('           { TOpenBracket      _ }
  ')'           { TCloseBracket     _ }
  '['           { TOpenSqrBracket   _ }
  ']'           { TCloseSqrBracket  _ }
  '='           { TAssign           _ }
  '+'           { TPlus             _ }
  '-'           { TMinus            _ }
  '*'           { TMult             _ }
  '/'           { TDiv              _ }
  '%'           { TMod              _ }
  '//'          { TFloatDiv         _ }

%%

Code
  : Statements                            { $1 }

Statements
  : NEWLINE Statements                    { $2 }
  | Statement NEWLINE Statements          { $1 : $3 }      
  |                                       { [] }

Statement
  : VAR '=' Expr                          { Assign (name $1) $3 }
  | Expr                                  { Procedure $1 }

Expr
  : Sum                                   { $1 }

Sum
  : Sum '+' Term                          { Plus $1 $3 }
  | Sum '-' Term                          { Minus $1 $3 }
  | Term                                  { $1 }

Term
  : Term '*' Atom                         { Mult $1 $3 }
  | Term '/' Atom                         { FloatDiv $1 $3 }
  | Term '//' Atom                        { Div $1 $3 }
  | Term '%' Atom                         { Mod $1 $3 }
  | Atom                                  { $1 }

Atom
  : VAR                                   { VarName $ name $1 }
  | INT                                   { IntValue $ value $1 }
  | Input                                 { $1 }

Input
  : VAR '(' ')'                           { Function (name $1) [] }
  | TYPE '(' Input ')'                    { Function (name $1) [$3] }



{

lexer = (alexMonadScan >>=)

data Statement = Assign String Expression | Procedure Expression deriving (Eq, Show)

data Expression 
  = Plus Expression Expression
  | Minus Expression Expression
  | Mult Expression Expression
  | FloatDiv Expression Expression
  | Div Expression Expression
  | Mod Expression Expression
  | Function String [Expression] 
  | VarName String
  | IntValue Integer
  deriving (Eq, Show)

parseError :: Token -> Alex a
parseError token = 
  case position token of
   (AlexPn _ line column) -> alexError $ "parse error at line " ++ (show line) ++ ", column " ++ (show column) ++ "with token" ++ (show token)

-- parse :: String -> Either String [Section]
parse s = runAlex s parseFile

}