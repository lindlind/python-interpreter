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




__________________

function_def_raw:
    | 'def' NAME '(' [params] ')' ['->' expression ] ':' [func_type_comment] block 