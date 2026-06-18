grammar Foo;

program
    : (moduleDef | comment | blank)* EOF
    ;

moduleDef
    : MODULE moduleName NEWLINE
      moduleItem*
      END NEWLINE?
    ;

// A module name may be a plain type (STR, MOLECULE), a generic type
// (VEC{INT}, MAP{INT,STR}), or a submodule qualified with a dot
// (MOLECULE.BASE, DIFFRACTION_DATA.INQ).
moduleName
    : typeSpec (DOT IDENTIFIER)*
    ;

moduleItem
    : useStmt
    | interfaceBlock
    | procDef
    | varDecl
    | containsStmt
    | implicitStmt
    | comment
    | blank
    ;

useStmt
    : INDENT3 USE IDENTIFIER NEWLINE
    ;

containsStmt
    : INDENT3 CONTAINS NEWLINE
    ;

implicitStmt
    : INDENT3 IMPLICIT NONE NEWLINE
    ;

interfaceBlock
    : INDENT3 INTERFACE IDENTIFIER NEWLINE
      interfaceItem*
      INDENT3 END NEWLINE?
    ;

interfaceItem
    : INDENT3 IDENTIFIER NEWLINE
    | comment
    | blank
    ;

procDef
    : procHeader NEWLINE
      signatureComment*
      procBody*
      INDENT3 END NEWLINE?
    ;

procHeader
    : INDENT3 IDENTIFIER procArgs? procResult? procAttrs?
    ;

procArgs
    : LPAREN identList? RPAREN
    ;

procResult
    : RESULT LPAREN IDENTIFIER RPAREN
    ;

procAttrs
    : TRIPLE_COLON attrList
    ;

attrList
    : attr (COMMA? attr)*
    ;

attr
    : GET_FROM LPAREN getFromArg (COMMA getFromArg)* RPAREN
    | PURE
    | ELEMENTAL
    | TEMPLATE
    | LEAKY
    | RECURSIVE
    | PRIVATE
    | SELFLESS
    | INLINED_BY_FOO
    | OPTIONAL
    | IN
    | OUT
    | INOUT
    ;

getFromArg
    : IDENTIFIER
    | IDENTIFIER QUESTION ARROW typeSpec
    | IDENTIFIER COLON IDENTIFIER
    ;

signatureComment
    : COMMENT NEWLINE?
    ;

procBody
    : localDecl
    | stmt
    | comment
    | blank
    ;

localDecl
    : INDENT6 identList DCOLON typeSpec attrSuffix? initSuffix? NEWLINE
    ;

varDecl
    : INDENT3 identList DCOLON typeSpec attrSuffix? initSuffix? NEWLINE
    ;

identList
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

attrSuffix
    : COMMA attr (COMMA attr)*
    ;

initSuffix
    : EQUAL expr
    ;

typeSpec
    : primitiveType
    | paramType
    | arrayType
    | IDENTIFIER
    ;

primitiveType
    : INT
    | REAL
    | CPX
    | BIN
    | STR
    | OBJECT
    | INTRINSIC
    | TYPES
    ;

paramType
    : IDENTIFIER LBRACE typeSpec (COMMA typeSpec)* RBRACE dimSpec?
    ;

arrayType
    : VEC LBRACE typeSpec RBRACE dimSpec?
    | MAT LBRACE typeSpec RBRACE dimSpec?
    | MAT3 LBRACE typeSpec RBRACE dimSpec?
    | MAT4 LBRACE typeSpec RBRACE dimSpec?
    | MAT5 LBRACE typeSpec RBRACE dimSpec?
    ;

dimSpec
    : LPAREN dimArg (COMMA dimArg)* RPAREN
    ;

dimArg
    : IDENTIFIER
    | INTEGER
    | STAR
    | COLON
    ;

stmt
    : INDENT6? simpleStmt NEWLINE
    ;

simpleStmt
    : assignment
    | callLike
    | controlStmt
    ;

assignment
    : lvalue EQUAL expr
    ;

lvalue
    : path
    ;

// Call forms, including module- and submodule-qualified calls.
//   foo(a)            plain / dotted call
//   .foo   .foo(a)    dot-method call on self
//   QUAL:foo(a)       explicit generic call            (single colon)
//   QUAL::foo(a)      explicit non-generic call        (double colon)
//   :foo(a)  ::foo(a) same-module call (qualifier omitted)
//   .SET:foo(a)       submodule generic call           (.SUBMOD:)
//   .SET::foo(a)      submodule non-generic call
//   .:foo(a)  .::foo  same-submodule call (qualifier omitted)
//   .MAIN:foo(a)      call into the main module from a submodule
callLike
    : path LPAREN argList? RPAREN
    | DOT path (LPAREN argList? RPAREN)?
    | DOT? qualifier? (DCOLON | COLON) IDENTIFIER (LPAREN argList? RPAREN)?
    ;

// The module, submodule or type name before a ':' or '::' in a qualified
// call. It may be a reserved type name (e.g. OBJECT:set, STR::proc).
qualifier
    : IDENTIFIER
    | primitiveType
    ;

argList
    : expr (COMMA expr)*
    ;

controlStmt
    : IF LPAREN expr RPAREN THEN
    | ELSE IF LPAREN expr RPAREN THEN
    | SELECT CASE LPAREN expr RPAREN
    | CASE DEFAULT
    | CASE LPAREN expr (COMMA expr)* RPAREN
    | DO loopHeader?
    | EXIT
    | CYCLE
    ;

loopHeader
    : IDENTIFIER EQUAL expr COMMA expr (COMMA expr)?
    ;

path
    : IDENTIFIER (DOT IDENTIFIER)*
    ;

expr
    : primary (binOp primary)*
    ;

primary
    : IDENTIFIER
    | INTEGER
    | REAL_NUMBER
    | STRING
    | TRUE
    | FALSE
    | NULL
    | ZERO
    | ONE
    | LPAREN expr RPAREN
    | callLike
    ;

binOp
    : PLUS
    | MINUS
    | STAR
    | SLASH
    | EQUAL_OP
    | NOT_EQUAL
    | LT
    | LE
    | GT
    | GE
    | AND
    | OR
    ;

comment
    : COMMENT NEWLINE?
    ;

blank
    : NEWLINE
    ;

MODULE : 'module' | 'MODULE' ;
END : 'end' | 'END' ;
USE : 'use' | 'USE' ;
INTERFACE : 'interface' | 'INTERFACE' ;
CONTAINS : 'contains' | 'CONTAINS' ;
IMPLICIT : 'implicit' | 'IMPLICIT' ;
NONE : 'none' | 'NONE' ;
RESULT : 'result' | 'RESULT' ;
GET_FROM : 'get_from' | 'GET_FROM' ;
PURE : 'pure' | 'PURE' ;
ELEMENTAL : 'elemental' | 'ELEMENTAL' ;
TEMPLATE : 'template' | 'TEMPLATE' ;
LEAKY : 'leaky' | 'LEAKY' ;
RECURSIVE : 'recursive' | 'RECURSIVE' ;
PRIVATE : 'private' | 'PRIVATE' ;
SELFLESS : 'selfless' | 'SELFLESS' ;
INLINED_BY_FOO : 'inlined_by_foo' | 'INLINED_BY_FOO' ;
OPTIONAL : 'optional' | 'OPTIONAL' ;
IN : 'IN' ;
OUT : 'OUT' ;
INOUT : 'INOUT' ;
IF : 'if' | 'IF' ;
THEN : 'then' | 'THEN' ;
ELSE : 'else' | 'ELSE' ;
SELECT : 'select' | 'SELECT' ;
CASE : 'case' | 'CASE' ;
DEFAULT : 'default' | 'DEFAULT' ;
DO : 'do' | 'DO' ;
EXIT : 'exit' | 'EXIT' ;
CYCLE : 'cycle' | 'CYCLE' ;
INT : 'INT' ;
REAL : 'REAL' ;
CPX : 'CPX' ;
BIN : 'BIN' ;
STR : 'STR' ;
OBJECT : 'OBJECT' ;
INTRINSIC : 'INTRINSIC' ;
TYPES : 'TYPES' ;
VEC : 'VEC' ;
MAT : 'MAT' ;
MAT3 : 'MAT3' ;
MAT4 : 'MAT4' ;
MAT5 : 'MAT5' ;

DCOLON : '::' ;
TRIPLE_COLON : ':::' ;
ARROW : '=>' ;
COLON : ':' ;
COMMA : ',' ;
LPAREN : '(' ;
RPAREN : ')' ;
LBRACE : '{' ;
RBRACE : '}' ;
PLUS : '+' ;
MINUS : '-' ;
STAR : '*' ;
SLASH : '/' ;
EQUAL : '=' ;
EQUAL_OP : '==' ;
NOT_EQUAL : '/=' ;
LT : '<' ;
LE : '<=' ;
GT : '>' ;
GE : '>=' ;
AND : '.and.' | 'AND' ;
OR : '.or.' | 'OR' ;
NOT : '.not.' | 'NOT' ;
DOT : '.' ;
QUESTION : '?' ;
TRUE : 'TRUE' | '.true.' ;
FALSE : 'FALSE' | '.false.' ;
NULL : 'NULL' | 'null' ;
ZERO : 'ZERO' ;
ONE : 'ONE' ;

IDENTIFIER
    : [a-zA-Z_][a-zA-Z0-9_]*
    ;

INTEGER
    : [0-9]+
    ;

REAL_NUMBER
    : [0-9]+ '.' [0-9]+ ([dDeE] [+-]? [0-9]+)?
    | [0-9]+ [dD] [+-]? [0-9]+
    ;

STRING
    : '"' (~["\r\n] | '\\' .)* '"'
    | '\'' (~['\r\n] | '\\' .)* '\''
    ;

COMMENT
    : '!' ~[\r\n]*
    ;

INDENT3
    : '   '
    ;

INDENT6
    : '      '
    ;

NEWLINE
    : '\r'? '\n'
    ;

WS
    : [ \t]+ -> skip
    ;
