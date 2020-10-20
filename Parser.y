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
  NEWLINE       { TNewline          _ _ }
  TYPE          { TType             _ _ name }
  BOOL          { TBool             _ _ bValue }
  STR           { TString           _ _ sValue }
  INT           { TInt              _ _ iValue }
  FLOAT         { TFloat            _ _ fValue }
  VAR           { TVariable         _ _ name }
  ","           { TComma            _ _ }
  "."           { TDot              _ _ }
  "("           { TOpenBracket      _ _ }
  ")"           { TCloseBracket     _ _ }
  "["           { TOpenSqrBracket   _ _ }
  "]"           { TCloseSqrBracket  _ _ }
  "="           { TAssign           _ _ }
  "or"          { TOr               _ _ }
  "and"         { TAnd              _ _ }
  "not"         { TNot              _ _ }
  "<"           { TLT               _ _ }
  ">"           { TGT               _ _ }
  "=="          { TEq               _ _ }
  "!="          { TNEq              _ _ }
  "<="          { TLTE              _ _ }
  ">="          { TGTE              _ _ }
  "|"           { TBitOr            _ _ }
  "^"           { TBitXor           _ _ }
  "&"           { TBitAnd           _ _ }
  "<<"          { TLeftShift        _ _ }
  ">>"          { TRightShift       _ _ }
  "+"           { TPlus             _ _ }
  "-"           { TMinus            _ _ }
  "**"          { TPower            _ _ }
  "*"           { TMult             _ _ }
  "//"          { TDiv              _ _ }
  "%"           { TMod              _ _ }
  "/"           { TFloatDiv         _ _ }
  ":"           { TSlice            _ _ }

%%
Code :: {StatementParse}
Code
  : Newlines Statements                                  { $2 }
  | Newlines                                             { EmptySeq }

Statements :: {StatementParse}
Statements
  : Statement NEWLINE Newlines Statements                { $1 `StmtPrsSeq` $4 }
  | Statement Newlines                                   { $1 }

Newlines
  : NEWLINE Newlines                                     { }
  |                                                      { }

Statement :: {StatementParse}
Statement
  : SmallStatement                                       { $1 }
--  | CompoundStatement                                    

SmallStatement :: {StatementParse}
  : Procedure                                            { $1 }
  | Assignment                                           { $1 }
--  | Return
--  | 'pass'
--  | 'break'
--  | 'continue'

Assignment :: {StatementParse}
Assignment
  : VAR "=" Expr                                         { AssignWT $1 $3 }

Procedure :: {StatementParse}
Procedure
  : Expr                                                 { ProcedureWT $1 }

Exprs :: {[ExprParse]}
Exprs
  : Expr "," Exprs                                       { $1 : $3 }
  | Expr                                                 { [$1] }

Expr :: {ExprParse}
Expr
  : Disj                                                 { $1 }

Disj :: {ExprParse}
Disj
  : Conj "or" Disj                                       { BinOpWT $1 $2 $3 }
  | Conj                                                 { $1 }

Conj :: {ExprParse}
Conj
  : Inv "and" Conj                                       { BinOpWT $1 $2 $3 }
  | Inv                                                  { $1 }

Inv :: {ExprParse}
Inv
  : "not" Inv                                            { UnOpWT $1 $2 }
  | Comp                                                 { $1 }

Comp :: {ExprParse}
Comp
  : BitwiseOr "==" BitwiseOr                             { BinOpWT $1 $2 $3 }
  | BitwiseOr "!=" BitwiseOr                             { BinOpWT $1 $2 $3 }
  | BitwiseOr "<" BitwiseOr                              { BinOpWT $1 $2 $3 }
  | BitwiseOr ">" BitwiseOr                              { BinOpWT $1 $2 $3 }
  | BitwiseOr "<=" BitwiseOr                             { BinOpWT $1 $2 $3 }
  | BitwiseOr ">=" BitwiseOr                             { BinOpWT $1 $2 $3 }
  | BitwiseOr                                            { $1 }

BitwiseOr :: {ExprParse}
BitwiseOr
  : BitwiseOr "|" BitwiseXor                             { BinOpWT $1 $2 $3 }
  | BitwiseXor                                           { $1 }

BitwiseXor :: {ExprParse}
BitwiseXor
  : BitwiseXor "^" BitwiseAnd                            { BinOpWT $1 $2 $3 }
  | BitwiseAnd                                           { $1 }

BitwiseAnd :: {ExprParse}
BitwiseAnd
  : BitwiseAnd "&" Shift                                 { BinOpWT $1 $2 $3 }
  | Shift                                                { $1 }

Shift :: {ExprParse}
Shift
  : Shift "<<" Sum                                       { BinOpWT $1 $2 $3 }
  | Shift ">>" Sum                                       { BinOpWT $1 $2 $3 }
  | Sum                                                  { $1 }

Sum :: {ExprParse}
Sum
  : Sum "+" Term                                         { BinOpWT $1 $2 $3 }
  | Sum "-" Term                                         { BinOpWT $1 $2 $3 }
  | Term                                                 { $1 }

Term :: {ExprParse}
Term
  : Term "*" Unary                                       { BinOpWT $1 $2 $3 }
  | Term "/" Unary                                       { BinOpWT $1 $2 $3 }
  | Term "//" Unary                                      { BinOpWT $1 $2 $3 }
  | Term "%" Unary                                       { BinOpWT $1 $2 $3 }
  | Unary                                                { $1 }

Unary :: {ExprParse}
Unary
  : "+" Unary                                            { UnOpWT $1 $2 }
  | "-" Unary                                            { UnOpWT $1 $2 }
  | Power                                                { $1 }

Power :: {ExprParse}
Power
  : Primary "**" Unary                                   { BinOpWT $1 $2 $3 }
  | Primary                                              { $1 }

Primary :: {ExprParse}
  : Atom                                                 { $1 }
--  | Primary "(" Arguments ")"                            { FuncWT $1 $3 }
--  | Primary "[" Slices "]"                               { SliceWT $1 $3 }

Arguments :: {[ExprParse]}
Arguments
  : Exprs                                                { $1 }
  |                                                      { [] }

Slices :: {[ExprParse]}
Slices
  : Expr ":" Expr ":" Expr                               { $1 : $3 : $5 : [] }
  | Expr ":" Expr                                        { $1 : $3 : [] }
  | Expr                                                 { $1 : [] }

Atom :: {ExprParse}
Atom
  : INT                                                  { AtomWT $1 }
  | FLOAT                                                { AtomWT $1 }
  | BOOL                                                 { AtomWT $1 }
  | STR                                                  { AtomWT $1 }
  | VAR                                                  { AtomWT $1 }
  | "(" Expr ")"                                         { RecWT $2 }


{

lexer = (alexMonadScan >>=)

data StatementParse = AssignWT Token ExprParse
                    | ProcedureWT ExprParse
                    | StmtPrsSeq StatementParse StatementParse
                    | EmptySeq
                    deriving (Eq, Show)

data ExprParse = BinOpWT ExprParse Token ExprParse
               | UnOpWT Token ExprParse
               | FuncWT ExprParse [ExprParse]
               | SliceWT ExprParse [ExprParse]
               | AtomWT Token
               | RecWT ExprParse
               deriving (Eq, Show)


parseError :: Token -> Alex a
parseError token =
  let
    (AlexPn _ line column) = position token
  in
    alexError $ "Syntax error: "
                  ++ "line " ++ (show line) ++ ", "
                  ++ "column " ++ (show column) ++ ", "
                  ++ "token " ++  "'" ++ (content token) ++ "'"

parse :: String -> Either String StatementParse
parse s = runAlex s parseFile

}