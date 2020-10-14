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
  BOOL          { TBool             _ bValue }
  STR           { TString           _ sValue }
  INT           { TInteger          _ iValue }
  FLOAT         { TFloat            _ fValue }
  VAR           { TVariable         _ name }
  ","           { TComma            _ }
  "."           { TDot              _ }
  "("           { TOpenBracket      _ }
  ")"           { TCloseBracket     _ }
  "["           { TOpenSqrBracket   _ }
  "]"           { TCloseSqrBracket  _ }
  "="           { TAssign           _ }
  "or"          { TOr               _ }
  "and"         { TAnd              _ }
  "not"         { TNot              _ }
  "<"           { TLT               _ }
  ">"           { TGT               _ }
  "=="          { TEq               _ }
  "!="          { TNEq              _ }
  "<="          { TLTE              _ }
  ">="          { TGTE              _ }
  "|"           { TBitOr            _ }
  "^"           { TBitXor           _ }
  "&"           { TBitAnd           _ }
  "<<"          { TLeftShift        _ }
  ">>"          { TRightShift       _ }
  "+"           { TPlus             _ }
  "-"           { TMinus            _ }
  "*"           { TMult             _ }
  "//"          { TDiv              _ }
  "%"           { TMod              _ }
  "/"           { TFloatDiv         _ }

%%
Code :: {[Statement]}
Code
  : Newlines Statements                                  { $2 }
  | Newlines                                             { [] }

Statements :: {[Statement]}
Statements
  : Statement NEWLINE Newlines Statements                { (Statement $1) : $4 }
  | Statement Newlines                                   { [Statement $1] }

Newlines
  : NEWLINE Newlines                                     { }
  |                                                      { }

Statement :: {forall expr. IExpr expr => expr ()}
Statement
  : Assign                                               { $1 }
  | Procedure                                            { $1 }

Assign :: {forall expr. IExpr expr => expr ()}
Assign
  : VAR "=" ExprBool                                     { iAssign (name $1) $3 }
  | VAR "=" ExprNum                                      { iAssign (name $1) $3 }

Procedure :: {forall expr. IExpr expr => expr ()}
Procedure
  : ExprBool                                             { iProcedure $1 }
  | ExprNum                                              { iProcedure $1 }

ExprBool :: {forall expr . IExpr expr => expr Bool}
ExprBool
  : Disj                                                 { $1 }

Disj :: {forall expr . IExpr expr => expr Bool}
Disj
  : Conj "or" Disj                                       { iOr $1 $3 }
  | Conj                                                 { $1 }

Conj :: {forall expr . IExpr expr => expr Bool}
Conj
  : Inv "and" Conj                                       { iAnd $1 $3 }
  | Inv                                                  { $1 }

Inv :: {forall expr . IExpr expr => expr Bool}
Inv
  : "not" Inv                                            { iNot $2 }
  | Comp                                                 { $1 }

Comp :: {forall expr . IExpr expr => expr Bool}
Comp
  : BoolComp                                             { $1 }
  | NumComp                                              { $1 } 

BoolComp :: {forall expr . IExpr expr => expr Bool}
BoolComp
  : AtomBool "==" AtomBool                               { iBoolEq $1 $3 }
  | AtomBool "!=" AtomBool                               { iBoolNEq $1 $3 }
  | AtomBool                                             { $1 }

AtomBool :: {forall expr . IExpr expr => expr Bool}
  : BOOL                                                 { iBoolVal $ bValue $1 }

NumComp :: {forall expr . IExpr expr => expr Bool}
NumComp
  : ExprNum "==" ExprNum                                 { iEq $1 $3 }
  | ExprNum "!=" ExprNum                                 { iNEq $1 $3 }
  | ExprNum "<" ExprNum                                  { iLT $1 $3 }
  | ExprNum ">" ExprNum                                  { iGT $1 $3 }
  | ExprNum "<=" ExprNum                                 { iLTE $1 $3 }
  | ExprNum ">=" ExprNum                                 { iGTE $1 $3 }

ExprNum :: {forall expr . IExpr expr => expr Number}
  : BitwiseOr                                            { $1 }

BitwiseOr :: {forall expr . IExpr expr => expr Number}
BitwiseOr
  : BitwiseOr "|" BitwiseXor                             { iBitOr $1 $3 }
  | BitwiseXor                                           { $1 }

BitwiseXor :: {forall expr . IExpr expr => expr Number}
BitwiseXor
  : BitwiseXor "^" BitwiseAnd                            { iBitXor $1 $3 }
  | BitwiseAnd                                           { $1 }

BitwiseAnd :: {forall expr . IExpr expr => expr Number}
BitwiseAnd
  : BitwiseAnd "&" Shift                                 { iBitAnd $1 $3 }
  | Shift                                                { $1 }

Shift :: {forall expr . IExpr expr => expr Number}
Shift
  : Shift "<<" Sum                                       { iLeftShift $1 $3 }
  | Shift ">>" Sum                                       { iRightShift $1 $3 }
  | Sum                                                  { $1 }

Sum :: {forall expr . IExpr expr => expr Number}
Sum
  : Sum "+" Term                                         { iPlus $1 $3 }
  | Sum "-" Term                                         { iMinus $1 $3 }
  | Term                                                 { $1 }

Term :: {forall expr . IExpr expr => expr Number}
Term
  : Term "*" AtomNum                                     { iMult $1 $3 }
  | Term "/" AtomNum                                     { iFloatDiv $1 $3 }
  | Term "//" AtomNum                                    { iDiv $1 $3 }
  | Term "%" AtomNum                                     { iMod $1 $3 }
  | AtomNum                                              { $1 }

AtomNum :: {forall expr . IExpr expr => expr Number}
AtomNum
  : INT                                                  { iIntVal $ iValue $1 }
  | FLOAT                                                { iFloatVal $ fValue $1 }


{

lexer = (alexMonadScan >>=)

newtype Statement = Statement (forall expr. IExpr expr => expr ())

data Number = IntNum   { getInt   :: Integer }
            | FloatNum { getFloat :: Float   }

class IExpr expr where
  iAssign    :: String -> expr t -> expr ()
  iProcedure :: expr t -> expr ()
  iOr      :: expr Bool -> expr Bool -> expr Bool
  iAnd     :: expr Bool -> expr Bool -> expr Bool
  iNot     :: expr Bool -> expr Bool
  iBoolEq  :: expr Bool -> expr Bool -> expr Bool
  iBoolNEq :: expr Bool -> expr Bool -> expr Bool
  iEq  :: expr Number -> expr Number -> expr Bool
  iNEq :: expr Number -> expr Number -> expr Bool
  iLT  :: expr Number -> expr Number -> expr Bool
  iGT  :: expr Number -> expr Number -> expr Bool
  iLTE :: expr Number -> expr Number -> expr Bool
  iGTE :: expr Number -> expr Number -> expr Bool
  iBitOr      :: expr Number -> expr Number -> expr Number
  iBitXor     :: expr Number -> expr Number -> expr Number
  iBitAnd     :: expr Number -> expr Number -> expr Number
  iLeftShift  :: expr Number -> expr Number -> expr Number
  iRightShift :: expr Number -> expr Number -> expr Number
  iPlus      :: expr Number -> expr Number -> expr Number
  iMinus     :: expr Number -> expr Number -> expr Number
  iMult      :: expr Number -> expr Number -> expr Number
  iFloatDiv  :: expr Number -> expr Number -> expr Number
  iDiv       :: expr Number -> expr Number -> expr Number
  iMod       :: expr Number -> expr Number -> expr Number
  iBoolVal  :: Bool    -> expr Bool
  iIntVal   :: Integer -> expr Number
  iFloatVal :: Float   -> expr Number

parseError :: Token -> Alex a
parseError token =
  case position token of
   (AlexPn _ line column) -> alexError $ "parse error at line " ++ (show line) ++ ", column " ++ (show column) ++ " with token " ++ (show token)

--parse :: IExpr expr => String -> Either String [expr ()]
parse s = runAlex s parseFile

}