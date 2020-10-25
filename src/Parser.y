{
module Parser
  ( ExprParse (..)
  , StatementParse (..)
  , parse
  ) where

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
  DEF           { TDef              _ _ }
  RETURN        { TReturn           _ _ }
  BREAK         { TBreak            _ _ }
  CONTINUE      { TContinue         _ _ }
  BOOL          { TBool             _ _ bVal }
  INT           { TInteger          _ _ iVal }
  FLOAT         { TFloat            _ _ fVal }
  STR           { TString           _ _ sVal }
  VAR           { TVariable         _ _ name }
  ","           { TComma            _ _ }
  "("           { TOpenBracket      _ _ }
  ")"           { TCloseBracket     _ _ }
  "["           { TOpenSqrBracket   _ _ }
  "]"           { TCloseSqrBracket  _ _ }
  "="           { TAssign           _ _ }
  "->"          { TArrow            _ _ }
  "or"          { TOr               _ _ }
  "and"         { TAnd              _ _ }
  "not"         { TNot              _ _ }
  COMP          { TComp             _ _ }
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
  | FunctionDef                                          { $1 }

IfStatement :: {StatementParse}
IfStatement
  : IF Expr ":" Block                                    { IfWT     (position $1) $2 $4 }
  | IF Expr ":" Block ElifStatement                      { IfElseWT (position $1) $2 $4 $5 }
  | IF Expr ":" Block ElseStatement                      { IfElseWT (position $1) $2 $4 $5 }

ElifStatement :: {StatementParse}
ElifStatement
  : ELIF Expr ":" Block                                  { IfWT     (position $1) $2 $4 }
  | ELIF Expr ":" Block ElifStatement                    { IfElseWT (position $1) $2 $4 $5 }
  | ELIF Expr ":" Block ElseStatement                    { IfElseWT (position $1) $2 $4 $5 }

ElseStatement :: {StatementParse}
ElseStatement
  : ELSE ":" Block                                       { $3 }

WhileStatement :: {StatementParse}
WhileStatement
  : WHILE Expr ":" Block                                 { WhileWT (position $1) $2 $4 }

FunctionDef :: {StatementParse}
FunctionDef
    : DEF VAR "("              ")" "->" TYPE ":" Block   { DefFunc0WT $2              $6  $8  }
    | DEF VAR "(" VAR ":" TYPE ")" "->" TYPE ":" Block   { DefFunc1WT $2 $4 $6        $9  $11 }
    | DEF VAR "(" VAR ":" TYPE 
              "," VAR ":" TYPE ")" "->" TYPE ":" Block   { DefFunc2WT $2 $4 $6 $8 $10 $12 $15 }

Block :: {StatementParse}
Block
  : NEWLINE INDENT Statements DEDENT                     { $3 } -- problem with multiple dedents

SmallStatement :: {StatementParse}
  : Procedure                                            { $1 }
  | Assignment                                           { $1 }
  | Print                                                { $1 }
  | Return                                               { $1 }
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

Return :: {StatementParse}
Return
  : RETURN Expr                                          { ReturnWT $2 }

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
  | FunctionCall                                         { $1 }
  | StringSlices                                         { $1 }

FunctionCall :: {ExprParse}
FunctionCall
    : VAR "("               ")"                          { CallFunc0WT $1 }
    | VAR "(" Expr          ")"                          { CallFunc1WT $1 $3 }
    | VAR "(" Expr "," Expr ")"                          { CallFunc2WT $1 $3 $5 }

StringSlices :: {ExprParse}
StringSlices
  : Primary "[" Expr          "]"                        { Slice1WT $2 $1 $3 }
  | Primary "[" Expr ":" Expr "]"                        { Slice2WT $2 $1 $3 $5 }

Atom :: {ExprParse}
Atom
  : BOOL                                                 { AtomWT $1 }
  | INT                                                  { AtomWT $1 }
  | FLOAT                                                { AtomWT $1 }
  | STR                                                  { AtomWT $1 }
  | VAR                                                  { AtomWT $1 }


{

lexer = (alexMonadScan >>=)

data StatementParse = IfWT     AlexPosn ExprParse StatementParse
                    | IfElseWT AlexPosn ExprParse StatementParse StatementParse
                    | WhileWT  AlexPosn ExprParse StatementParse
                    | BreakWT
                    | ContinueWT
                    | DefFunc0WT Token                         Token StatementParse
                    | DefFunc1WT Token Token Token             Token StatementParse
                    | DefFunc2WT Token Token Token Token Token Token StatementParse
                    | ReturnWT ExprParse
                    | ProcedureWT ExprParse
                    | AssignWT Token ExprParse
                    | PrintWT ExprParse
                    | StmtPrsSeq StatementParse StatementParse
                    | EmptySeq
                    deriving (Eq, Show)

data ExprParse = BinOpWT ExprParse Token ExprParse
               | UnOpWT Token ExprParse
               | CastWT Token ExprParse
               | CallFunc0WT Token
               | CallFunc1WT Token ExprParse
               | CallFunc2WT Token ExprParse ExprParse
               | Slice1WT Token ExprParse ExprParse
               | Slice2WT Token ExprParse ExprParse ExprParse
               | AtomWT Token
               | InputWT
               | RecWT ExprParse
               deriving (Eq, Show)

parseError :: Token -> Alex a
parseError TEof = alexError "Unexpected end of file (last empty line may be missed)"
parseError token =
  let
    (AlexPn _ line column) = position token
  in
    alexError $ "Syntax error: "
                  ++ "line " ++ (show line) ++ ", "
                  ++ "column " ++ (show column) ++ ", "
                  ++ "token " ++  "'" ++ (content token) ++ "'"

-- | Function converts python code from string to eDSL.
parse :: String -> Either String StatementParse
parse s = runAlex s parseFile

}
