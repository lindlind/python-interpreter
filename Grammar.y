Code
    : 
    | Statements

Statements
    : Statement NEWLINE Statements
    | Statement

Statement
    : SmallStatement
    | CompoundStatement

SmallStatement
    : Assignment
    | Return
    | 'pass'
    | 'break'
    | 'continue'

CompoundStatement
    : IfStatement
    | WhileStatement
    | FunctionDefinition

Assignment
    : Variables '=' Expressions
    | Variable augAssign Expression

Return
    : 'return' Expression
    | 'return'

WhileStatement
    : 'while' Expression ':' Block
    | 'while' Expression ':' Block ElseStatement

IfStatement
    : 'if' Expression ':' Block 
    | 'if' Expression ':' Block ElifStatement
    | 'if' Expression ':' Block ElseStatement

ElifStatement
    : 'elif' Expression ':' Block 
    | 'elif' Expression ':' Block ElifStatement
    | 'elif' Expression ':' Block ElseStatement

ElseStatement
    : 'else' ':' Block

FunctionDefinition
    : 'def' Variable '(' FunctionParameters ')' ':' Block

FunctionParameters
    : Expressions
    |

Block
    : NEWLINE INDENT Statements DEDENT

Expressions
    : Expression ',' Expressions
    | Expression

Expression
    : Disjunction

Disjunction
    : Conjunction 'or' Disjunction
    | Conjunction

Conjunction
    : Inversion 'and' Conjunction
    | Inversion

Inversion
    : 'not' Inversion
    | Comparison

Comparison
    : BitwiseOr compOp BitwiseOr
    | BitwiseOr

BitwiseOr
    : BitwiseOr '|' BitwiseXor
    | BitwiseXor

BitwiseXor
    : BitwiseXor '^' BitwiseAnd
    | BitwiseAnd

BitwiseAnd
    : BitwiseAnd '&' ShiftExpr
    | ShiftExpr

ShiftExpr
    : ShiftExpr '<<' Sum
    | ShiftExpr '>>' Sum
    | Sum

Sum
    : Sum '+' Term
    | Sum '-' Term
    | Term

Term
    : Term '*' Factor
    | Term '/' Factor 
    | Term '//' Factor 
    | Term '%' Factor 
    | Factor

Factor
    : '+' Factor
    | '-' Factor
    | '~' Factor
    | Power

Power
    : Primary ** Factor
    | Primary 

Primary
    : Primary '.' Variable
    | Primary '(' Arguments ')'
    | Primary '[' Slices ']'
    | Atom

Arguments
    : Expressions
    |

Atom
    : Variable
    | 'True' 
    | 'False' 
    | 'None' 
    | string
    | number

Variables
    : Variable ',' Variables
    | Variable

Variable
    : name
