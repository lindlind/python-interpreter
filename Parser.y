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
  INDENT        { TIndent           _ _ }
  DEDENT        { TDedent           _ _ }
  TYPE          { TType             _ _ name }
  INPUT         { TInput            _ _ }
  PRINT         { TPrint            _ _ }
  WHILE         { TWhile            _ _ }
  IF            { TIf               _ _ }
  ELIF          { TElif             _ _ }
  ELSE          { TElse             _ _ }
  RETURN        { TReturn           _ _ }
  BREAK         { TBreak            _ _ }
  CONTINUE      { TContinue         _ _ }
  BOOL          { TBool             _ _ bValue }
  INT           { TInteger          _ _ iValue }
  FLOAT         { TFloat            _ _ fValue }
  STR           { TString           _ _ sValue }
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
  COMP          { TComp             _ _ op }
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
  : Statements                                           { $1 }
  |                                                      { EmptySeq }

Statements :: {StatementParse}
Statements
  : Statement Statements                                 { $1 `StmtPrsSeq` $2 }
  | Statement                                            { $1 }

Statement :: {StatementParse}
Statement
  : SmallStatement NEWLINE                               { $1 }
  | CompoundStatement                                    { $1 }

CompoundStatement :: {StatementParse}
CompoundStatement
  : IfStatement                                          { $1 }
  | WhileStatement                                       { $1 }
--  | FunctionDef                                        { $1 }

WhileStatement :: {StatementParse}
WhileStatement
  : WHILE Expr ":" Block                                 { WhileWT $2 $4 }

IfStatement :: {StatementParse}
IfStatement
  : IF Expr ":" Block                                    { IfWT $2 $4 }
  | IF Expr ":" Block ElifStatement                      { IfElseWT $2 $4 $5 }
  | IF Expr ":" Block ElseStatement                      { IfElseWT $2 $4 $5 }

ElifStatement :: {StatementParse}
ElifStatement
  : ELIF Expr ":" Block                                  { IfWT $2 $4 }
  | ELIF Expr ":" Block ElifStatement                    { IfElseWT $2 $4 $5 }
  | ELIF Expr ":" Block ElseStatement                    { IfElseWT $2 $4 $5 }

ElseStatement :: {StatementParse}
ElseStatement
  : ELSE ":" Block                                       { $3 }

Block :: {StatementParse}
Block
  : NEWLINE INDENT Statements DEDENT                     { $3 } -- problem with multiple dedents

SmallStatement :: {StatementParse}
  : Procedure                                            { $1 }
  | Assignment                                           { $1 }
  | Print                                                { $1 }
--  | Return                                               { $1 }
  | BREAK                                                { BreakWT }
  | CONTINUE                                             { ContinueWT }

Assignment :: {StatementParse}
Assignment
  : VAR "=" Expr                                         { AssignWT $1 $3 }

Procedure :: {StatementParse}
Procedure
  : Expr                                                 { ProcedureWT $1 }

Print :: {StatementParse}
Print
  : PRINT "(" Expr ")"                                   { PrintWT $3 }

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
  : BitwiseOr COMP BitwiseOr                             { BinOpWT $1 $2 $3 }
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
  | INPUT "(" ")"                                        { InputWT }
  | "(" Expr ")"                                         { RecWT $2 }
  | TYPE "(" Expr ")"                                    { CastWT $1 $3 }
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
  : BOOL                                                 { AtomWT $1 }
  | INT                                                  { AtomWT $1 }
  | FLOAT                                                { AtomWT $1 }
  | STR                                                  { AtomWT $1 }
  | VAR                                                  { AtomWT $1 }


{

lexer = (alexMonadScan >>=)

data StatementParse = IfWT ExprParse StatementParse
                    | IfElseWT ExprParse StatementParse StatementParse
                    | WhileWT ExprParse StatementParse
                    | ProcedureWT ExprParse
                    | AssignWT Token ExprParse
                    | PrintWT ExprParse
                    | BreakWT
                    | ContinueWT
                    | StmtPrsSeq StatementParse StatementParse
                    | EmptySeq
                    deriving (Eq, Show)

data ExprParse = BinOpWT ExprParse Token ExprParse
               | UnOpWT Token ExprParse
               | CastWT Token ExprParse
               | FuncWT ExprParse [ExprParse]
               | SliceWT ExprParse [ExprParse]
               | AtomWT Token
               | InputWT
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