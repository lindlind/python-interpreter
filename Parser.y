{
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

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
  INT           { TInteger          _ iValue }
  FLOAT         { TFloat            _ fValue }
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
  '//'          { TDiv              _ }
  '%'           { TMod              _ }
  '/'           { TFloatDiv         _ }

%%
Code :: {[Statement]}
Code
  : Statements                            { $1 }

Statements :: {[Statement]}
Statements
  : NEWLINE Statements                    { $2 }
  | Statement NEWLINE Statements          { (Statement $1) : $3 }      
  |                                       { [] }

Statement :: {forall expr. IExpr expr => (expr ())}
Statement
  : VAR '=' Expr                          { iAssign (name $1) $3 }
  | Expr                                  { iProcedure $1 }

Expr :: {forall expr. IExpr expr => expr Float}
Expr
  : Sum                                   { $1 }

Sum :: {forall expr. IExpr expr => expr Float}
Sum
  : Sum '+' Term                          { iPlus $1 $3 }
  | Sum '-' Term                          { iMinus $1 $3 }
  | Term                                  { $1 }

Term :: {forall expr. IExpr expr => expr Float}
Term
  : Term '*' Atom                         { iMult $1 $3 }
  | Term '/' Atom                         { iFloatDiv $1 $3 }
  | Term '//' Atom                        { iDiv $1 $3 }
  | Term '%' Atom                         { iMod $1 $3 }
  | Atom                                  { $1 }

Atom :: {forall expr. IExpr expr => expr Float}
Atom
  : FLOAT                                 { iVal $ fValue $1 }


{

lexer = (alexMonadScan >>=)

newtype Statement = Statement (forall expr. IExpr expr => expr ())

class IExpr expr where
  iAssign    :: String -> expr t -> expr ()
  iProcedure :: expr t -> expr ()
  iPlus      :: expr Float -> expr Float -> expr Float
  iMinus     :: expr Float -> expr Float -> expr Float
  iMult      :: expr Float -> expr Float -> expr Float
  iFloatDiv  :: expr Float -> expr Float -> expr Float
  iDiv       :: expr Float -> expr Float -> expr Float
  iMod       :: expr Float -> expr Float -> expr Float
  iVal       :: Float -> expr Float

parseError :: Token -> Alex a
parseError token = 
  case position token of
   (AlexPn _ line column) -> alexError $ "parse error at line " ++ (show line) ++ ", column " ++ (show column) ++ " with token " ++ (show token)

--parse :: IExpr expr => String -> Either String [expr ()]
parse s = runAlex s parseFile

}