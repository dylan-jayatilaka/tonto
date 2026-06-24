grammar Foo;

// ---------------------------------------------------------------------------
// Block model
//
// Foo blocks are delimited by `end` keywords, NOT by indentation. Real sources
// indent inconsistently (e.g. a declaration at 7 spaces beside a sibling at 6,
// a `do` body at 14 under a `do` at 12), so whitespace is insignificant here:
// every block (module / procedure / interface / if / do / select) runs to its
// matching `end`; an if-body runs to `else`/`else if`/`end`, and a case-body to
// the next `case`/`end`.
//
// Statements are terminated by NEWLINE (';' separates several on one line).
// Continuation lines (ending in '&') are joined by the lexer. Comments ('!')
// and preprocessor lines ('#') go on the hidden channel; the translator
// recovers them by token position.
// ---------------------------------------------------------------------------

// ===========================================================================
// Parser
// ===========================================================================

// Every Foo file is one module — a normal `module X` or a `virtual module X`
// (a get_from template, never compiled to its own .F90). Both are moduleDef
// (the optional leading IDENTIFIER carries the `virtual` modifier).
program
    : (moduleDef | NEWLINE)* EOF
    ;

// A Foo module has a data (specification) section, then `contains`, then a
// procedure section — like a Fortran module. Either section may be empty, and
// a pure-data module may omit `contains` entirely.
// A module, optionally prefixed with a modifier word such as `virtual`.
moduleDef
    : IDENTIFIER? MODULE moduleName NEWLINE
      moduleDataItem*
      ( CONTAINS NEWLINE moduleProcItem* )?
      endKw? NEWLINE?
    ;

// A module name may be a plain type (STR, MOLECULE), a generic type
// (VEC{INT}, MAP{INT,STR}), or a submodule qualified with a dot
// (MOLECULE.BASE, DIFFRACTION_DATA.INQ).
moduleName
    : typeSpec (DOT IDENTIFIER)*
    ;

moduleDataItem
    : useStmt
    | implicitStmt
    | interfaceBlock
    | typeDef
    | dataStmt
    | varDecl
    | NEWLINE
    ;

// A derived-type definition: `type EVEC{BIN} … end`, optionally prefixed with a
// modifier word such as `array` (`array type VEC{EVEC{BIN}}`).
typeDef
    : IDENTIFIER? TYPE typeSpec NEWLINE
      (varDecl | NEWLINE)*
      endKw NEWLINE?
    ;

moduleProcItem
    : procDef
    | NEWLINE
    ;

// use MODULE  /  use MODULE, only: a, b, c   (module may be a type-keyword name)
useStmt
    : USE moduleRef (COMMA name COLON name (COMMA name)*)? NEWLINE
    ;

moduleRef
    : IDENTIFIER
    | primitiveType
    ;

implicitStmt
    : IMPLICIT NONE NEWLINE
    ;

// An interface block has two forms:
//   named   (`interface trim`)  — lists generic procedure names;
//   unnamed (`interface`)       — inside a procedure, declares dummy-procedure
//                                 arguments as full definitions (header, the
//                                 argument declarations, and `end`).
interfaceBlock
    : INTERFACE IDENTIFIER NEWLINE genericItem* endKw NEWLINE?
    | INTERFACE NEWLINE abstractItem* endKw NEWLINE?
    ;

// A named interface lists generic procedure names — one per line, or several
// comma-separated on a line (`get_item, get_bin, get_int, …`).
genericItem
    : procHeader (COMMA procHeader)* NEWLINE
    | NEWLINE
    ;

abstractItem
    : procDef
    | NEWLINE
    ;

procDef
    : procHeader NEWLINE
      procBody*
      endKw NEWLINE?
    ;

procHeader
    : IDENTIFIER procArgs? procResult? procAttrs?
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

// Attributes are separated by commas or whitespace; a stray '.' between two
// attributes (`leaky. PURE`) is tolerated, matching foo.pl's leniency.
attrList
    : attr ((COMMA | DOT)? attr)*
    ;

// An attribute: get_from(...), the intent keywords, or any identifier
// (optionally with a dimension spec, e.g. dimension(n)). Accepting a bare
// identifier covers the open-ended attribute vocabulary (public, allocatable,
// readonly, target, pointer, save, virtual, functional, …), lets those words
// still be used as ordinary names/components, and tolerates the occasional
// source typo (e.g. `privateo`) — matching foo.pl's leniency.
attr
    : GET_FROM LPAREN getFromArg (COMMA getFromArg)* RPAREN
    | IN
    | OUT
    | INOUT
    | name dimSpec?
    ;

// get_from(MODULE, KEY=>VALUE, ...). The module may be a reserved type name
// (e.g. INTRINSIC); a value may be '*'.
// The value after '=>' may be a type, an expression, an operator token
// (`EQ=>==`), '*', or empty (`CAST=>`, `CONJG=>`) — these are macro
// substitutions, so almost anything (or nothing) can appear.
getFromArg
    : getFromKey QUESTION? ARROW getFromVal?
    | name EQUAL getFromVal
    | typeSpec COLON IDENTIFIER
    | typeSpec
    | expr
    ;

// The key before '=>' is usually a type-like name, but may be an operator
// keyword (`EQ=>==`) used as a macro parameter name.
getFromKey
    : typeSpec
    | binOp
    ;

getFromVal
    : typeSpec
    | expr
    | binOp
    ;

procBody
    : localDecl
    | dataStmt
    | useStmt
    | interfaceBlock
    | stmt
    | NEWLINE
    ;

localDecl
    : identList DCOLON declTail NEWLINE
    ;

varDecl
    : identList DCOLON declTail NEWLINE
    ;

// The part after '::'. Usually a type (optionally pointer/allocatable, with
// trailing attributes and an initializer). It may also be attributes only —
// e.g. `self :: IN` — where the type is implicit (the enclosing class).
declTail
    : typeSpec ptrSuffix? attrSuffix? initSuffix?
    | attr (COMMA attr)* initSuffix?
    ;

// Pointer ('*') and allocatable ('@') type suffixes: INT*, VEC{REAL}@
ptrSuffix
    : STAR
    | AT
    ;

// Fortran-style data statement: data name(dims)/ v1, v2, ... /
// 'data' is a soft keyword (also a valid component/variable name, see `name`);
// a `data` line followed by '::' is a declaration, by a name a data statement.
dataStmt
    : DATA name dimSpec? SLASH dataValue (COMMA dataValue)* COMMA? SLASH NEWLINE
    ;

dataValue
    : MINUS? (literal | name)
    ;

// Declared names, each optionally with dimensions, e.g.
// `coefficients((l_max+1)*(l_max+1)) :: CPX`.
identList
    : declName (COMMA declName)*
    ;

declName
    : name dimSpec?
    ;

// A name that may be a value / variable / component. 'end' is a soft keyword:
// it is also a common variable name (e.g. `end = 0`, `self(end+1:)`,
// `end,f,l,last :: INT`) and is distinguished from a block-closing `end` by the
// token that follows it (a variable `end` is followed by '=', ',', '(', an
// operator, etc.; a block `end` by a newline).
name
    // IDENTIFIER, optionally with a trailing `?` placeholder, and also with
    // placeholders embedded mid-name: make_Hirshfeld?_atom_ED_grid (a get_from
    // key spliced into a routine name). The (QUESTION IDENTIFIER)* glues the
    // pieces; nameText keeps the embedded '?' so applySubst can substitute it.
    : IDENTIFIER (QUESTION IDENTIFIER)* QUESTION?
    // `end` is a soft keyword (variable `end`, `end+1`, `end,f,l :: INT`); but a
    // block-closing `end` is followed by a NEWLINE, or by a block keyword in the
    // explicit forms `end do` / `end if` / `end select` ... Excluding those lets a
    // block body terminate at its `end` instead of swallowing it as a statement
    // (e.g. `end do` was matching doStmt's `name? DO` as opening a loop).
    | {_input.LA(2) != NEWLINE
        && _input.LA(2) != FooParser.IF && _input.LA(2) != FooParser.DO
        && _input.LA(2) != FooParser.SELECT && _input.LA(2) != FooParser.INTERFACE
        && _input.LA(2) != FooParser.MODULE && _input.LA(2) != FooParser.TYPE
        && _input.LA(2) != FooParser.FORALL}? END
    | DATA
    | RESULT
    | TYPE
    | CASE
    | WHERE
    | DEFAULT
    ;

// Block terminator, covering the spaced forms `end if` / `end do` /
// `end select` / `end interface` (glued `endif`/`enddo` are one END token).
endKw
    : END (IF | DO | SELECT | INTERFACE | MODULE | TYPE | FORALL)?
    ;

// Trailing attributes after a type. Separators are commas or whitespace: the
// first attribute may follow the type with no comma, as the DEFAULT(...) macro
// does (`n_keys :: INT  DEFAULT(0)`).
attrSuffix
    : (COMMA? attr)+
    ;

initSuffix
    : EQUAL expr
    ;

// Any type may carry a trailing dimension / parameter spec, e.g.
// STR(len=52), VEC{REAL}(0:6), MAT{REAL}(3,4).
typeSpec
    : baseType QUESTION? dimSpec?
    ;

baseType
    : primitiveType
    | paramType
    | arrayType
    | TYPE
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
    ;

paramType
    : IDENTIFIER LBRACE typeSpec (COMMA typeSpec)* RBRACE
    ;

arrayType
    : VEC LBRACE typeSpec RBRACE
    | MAT LBRACE typeSpec RBRACE
    | MAT3 LBRACE typeSpec RBRACE
    | MAT4 LBRACE typeSpec RBRACE
    | MAT5 LBRACE typeSpec RBRACE
    | MAT6 LBRACE typeSpec RBRACE
    | MAT7 LBRACE typeSpec RBRACE
    ;

dimSpec
    : LPAREN dimArg (COMMA dimArg)* RPAREN
    ;

// Dimension / type-parameter arguments:
//   10            extent
//   3,4           several extents
//   0:6  1:n  :n  n:  :   range bounds
//   len=256       keyword parameter (string length etc.)
//   *             assumed size
dimArg
    : IDENTIFIER EQUAL expr
    | expr? COLON expr?
    | expr
    | STAR
    ;

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

stmt
    : ifStmt
    | doStmt
    | forallStmt
    | selectStmt
    | whereStmt
    | simpleLine
    ;

// forall (i=1:n) … end   /   forall (i=1:n) stmt
forallStmt
    : FORALL LPAREN forallHeader RPAREN NEWLINE procBody* endKw NEWLINE?
    | FORALL LPAREN forallHeader RPAREN simpleStmt NEWLINE
    ;

forallHeader
    : IDENTIFIER EQUAL expr COLON expr (COLON expr)?
    ;

// One source line: several statements separated by ';'. A one-line `if`/`where`
// guard (`if (done) exit`, `n = m; if (big) call x`) is allowed at any position.
simpleLine
    : lineStmt (SEMI lineStmt?)* NEWLINE
    ;

lineStmt
    : oneLineIf
    | oneLineWhere
    | simpleStmt
    ;

oneLineIf
    : IF LPAREN expr RPAREN simpleStmt
    ;

oneLineWhere
    : WHERE LPAREN expr RPAREN simpleStmt
    ;

// A simple statement is a postfix expression, optionally an assignment to it or
// an I/O tail (`read(...) value`, `write(...) self`), or a one-word control
// statement.
simpleStmt
    : {("write".equalsIgnoreCase(_input.LT(1).getText()) || "read".equalsIgnoreCase(_input.LT(1).getText()))
        && _input.LT(2).getType()==LPAREN}?
        name LPAREN argList? RPAREN ioTail?          // bare read/write = Fortran I/O: pass through, expand exprs
    | postfix (EQUAL expr | ARROW expr | ioTail)?
    | EXIT name?
    | CYCLE name?
    | RETURN
    ;

// The output/input list following an I/O control list, e.g. the `value` in
// `read(unit=self,fmt=*) value`.
ioTail
    : arg (COMMA arg)*
    ;

// A block body (then-part, else-part, do-body) ends at the next block keyword
// (else / else if / end) or the matching `end`. Both the indented form and the
// compact `then; stmt` / `else; stmt` inline form are supported.
ifStmt
    : IF LPAREN expr RPAREN THEN inlineBody
      elseIfClause*
      elseClause?
      endKw NEWLINE?
    ;

// A then/else body: optional inline statements after ';' (`then; res=1`), then a
// newline and an optional indented block. Handles `then;` with an empty inline
// part followed by a block, too.
inlineBody
    : (SEMI simpleStmt?)* NEWLINE procBody*
    ;

elseIfClause
    : ELSEIF LPAREN expr RPAREN THEN inlineBody
    ;

elseClause
    : ELSE inlineBody
    ;

// A do loop, optionally named (`outer: do … end do outer`), counted
// (`do i=1,n`), conditional (`do while (cond)`), or infinite (`do`).
// `do`, optionally named (`outer: do`) and/or prefixed with a modifier word
// such as `parallel` (`parallel do q = 1,n`).
doStmt
    : (name COLON)? name? DO (loopHeader | WHILE LPAREN expr RPAREN)? NEWLINE
      procBody*
      endKw name? NEWLINE?
    ;

loopHeader
    : IDENTIFIER EQUAL expr COMMA expr (COMMA expr)?
    ;

selectStmt
    : SELECT CASE LPAREN expr RPAREN NEWLINE
      (caseClause | NEWLINE)*
      endKw NEWLINE?
    ;

// A masked-assignment block:  where (mask) … [elsewhere (mask2) …] [elsewhere …] end
// Mirrors ifStmt (inlineBody handles both `where (m)\n…` and the compact
// `where (m); stmt` / `elsewhere; stmt` forms). The space-separated one-liner
// `where (m) stmt` is handled by oneLineWhere within lineStmt. The closing `end`
// is rendered as `end where`.
whereStmt
    : WHERE LPAREN expr RPAREN inlineBody
      elsewhereClause*
      endKw NEWLINE?
    ;

elsewhereClause
    : ELSEWHERE (LPAREN expr RPAREN)? inlineBody
    ;

// A case clause: an inline body (`case (0); res=1`), a block body
// (`case default` newline …), or both forms with a trailing ';'. The body is
// non-greedy so a following `case (…)` line starts a new clause rather than
// being absorbed as a statement (CASE is also a valid identifier, e.g.
// `present(case)`, so it can otherwise look like a call).
caseClause
    : caseLabel (SEMI simpleStmt?)* NEWLINE procBody*?
    ;

caseLabel
    : CASE LPAREN arg (COMMA arg)* RPAREN
    | CASE DEFAULT
    ;

// Expressions are postfix chains: a head followed by selectors ('.x' / '%x')
// and call/subscript parentheses. This covers chained and postfix calls such
// as (self+m).factorial, a(i).method(j), foo().bar.
expr
    : postfix (binOp postfix)*
    ;

postfix
    : head trailer*
    ;

head
    : LPAREN argList RPAREN
    | arrayConstructor
    | NOT postfix
    | MINUS postfix
    | PLUS postfix
    | callHead
    | literal
    ;

// The start of a name/call, including module- and submodule-qualified forms:
//   foo  .foo            plain name / dot-method on self
//   QUAL:foo  QUAL::foo  explicit generic / non-generic call
//   :foo  ::foo          same-module call (qualifier omitted)
//   .SET:foo .SET::foo   submodule call;  .:foo .::foo same submodule
//   .MAIN:foo            call into the main module from a submodule
callHead
    : DOT? qualifier? (DCOLON | COLON) name
    | DOT name
    | name
    ;

// A trailer: a component/method selector ('%' is a synonym for '.', used where
// legacy Foo could not decide component-vs-method by type), or a call/subscript.
trailer
    : (DOT | PERCENT) name (DCOLON | COLON) name
    | (DOT | PERCENT) (DCOLON | COLON) name
    | (DOT | PERCENT) name
    | LPAREN argList? RPAREN
    | LBRACKET argList? RBRACKET
    ;

// The module, submodule or type name before a ':' or '::' in a qualified call.
// It may be a reserved type name or a generic type (OBJECT:set, STR::proc,
// MAT{REAL}:trace_product_with).
qualifier
    : typeSpec
    ;

argList
    : arg (COMMA arg)*
    ;

// An argument: a keyword argument (last=l, fmt=*), a bare '*', an expression, or
// an array-section range incl. strides (a, a:b, a:b:c, :, :b). Implied-do I/O
// lists — `(x, i=1,n)`, even nested `((j, i=1,n), j=1,m)` — are absorbed here:
// the loop control `i=1` reads as a keyword argument, so no dedicated rule (and
// its LPAREN ambiguity, which caused catastrophic parse-time blowup) is needed.
arg
    : name EQUAL (expr | STAR)
    | STAR
    | expr (COLON expr? (COLON expr?)?)?
    | COLON expr? (COLON expr?)?
    ;

literal
    : INTEGER
    | REAL_NUMBER
    | STRING
    | BOZ
    | TRUE
    | FALSE
    | NULL
    | ZERO
    | ONE
    ;

// Array constructor: ["'",'"',"{"], the old-style (/ 1, 2, 3 /), or with an
// implied-do element [(expr, i=1,n)].
// Modern bracket form only: `[a, b, (expr, i=1,n)]`. (The legacy `(/ … /)`
// form has been replaced by `[ … ]` throughout the sources.) A bracketed group
// in trailer position instead means encapsulated-element access (a(i)[j] ->
// a(i)%element(j)); that is handled in `trailer`, distinguished by position.
arrayConstructor
    : LBRACKET (acElem (COMMA acElem)*)? RBRACKET
    ;

acElem
    : LPAREN expr COMMA loopHeader RPAREN
    | expr
    ;

binOp
    : PLUS
    | MINUS
    | POWER
    | STAR
    | SLASH
    | CONCAT
    | EQUAL_OP
    | NOT_EQUAL
    | LT
    | LE
    | GT
    | GE
    | AND
    | OR
    | EQV
    | NEQV
    | EQ
    | NE
    | LT_OP
    | LE_OP
    | GT_OP
    | GE_OP
    ;

// ===========================================================================
// Lexer
// ===========================================================================

MODULE : 'module' | 'MODULE' ;
// `end`, plus the glued block-end forms `endif` / `enddo`. (Maximal munch keeps
// longer identifiers like `endpoint` intact.) Spaced forms `end if` / `end do`
// are handled by the `endKw` parser rule.
END : ('end' | 'END') (('if' | 'IF') | ('do' | 'DO'))? ;
USE : 'use' | 'USE' ;
INTERFACE : 'interface' | 'INTERFACE' ;
CONTAINS : 'contains' | 'CONTAINS' ;
IMPLICIT : 'implicit' | 'IMPLICIT' ;
NONE : 'none' | 'NONE' ;
RESULT : 'result' | 'RESULT' ;
GET_FROM : 'get_from' | 'GET_FROM' ;
// Most attribute words (pure, elemental, private, public, allocatable,
// readonly, target, pointer, save, dimension, …) are NOT reserved: they are
// ordinary identifiers handled by the `attr` rule, so they can also be used as
// names/components. Only the intent keywords are reserved.
IN : 'IN' ;
OUT : 'OUT' ;
INOUT : 'INOUT' ;
IF : 'if' | 'IF' ;
THEN : 'then' | 'THEN' ;
WHILE : 'while' | 'WHILE' ;
WHERE : 'where' | 'WHERE' ;
FORALL : 'forall' | 'FORALL' ;
// `else if` (spaced) and `elseif` (glued) both lex as one ELSEIF token.
ELSEIF : ('else' | 'ELSE') [ \t]* ('if' | 'IF') ;
ELSEWHERE : 'elsewhere' | 'ELSEWHERE' ;
ELSE : 'else' | 'ELSE' ;
SELECT : 'select' | 'SELECT' ;
CASE : 'case' | 'CASE' ;
DEFAULT : 'default' | 'DEFAULT' ;
DO : 'do' | 'DO' ;
EXIT : 'exit' | 'EXIT' ;
CYCLE : 'cycle' | 'CYCLE' ;
RETURN : 'return' | 'RETURN' ;
DATA : 'data' | 'DATA' ;
INT : 'INT' ;
REAL : 'REAL' ;
CPX : 'CPX' ;
BIN : 'BIN' ;
STR : 'STR' ;
OBJECT : 'OBJECT' ;
INTRINSIC : 'INTRINSIC' ;
TYPE : 'type' | 'TYPE' ;
VEC : 'VEC' ;
MAT : 'MAT' ;
MAT3 : 'MAT3' ;
MAT4 : 'MAT4' ;
MAT5 : 'MAT5' ;
MAT6 : 'MAT6' ;
MAT7 : 'MAT7' ;

DCOLON : '::' ;
TRIPLE_COLON : ':::' ;
ARROW : '=>' ;
COLON : ':' ;
COMMA : ',' ;
LPAREN : '(' ;
RPAREN : ')' ;
LBRACE : '{' ;
RBRACE : '}' ;
LBRACKET : '[' ;
RBRACKET : ']' ;
SEMI : ';' ;
AT : '@' ;
PERCENT : '%' ;
PLUS : '+' ;
MINUS : '-' ;
POWER : '**' ;
STAR : '*' ;
SLASH : '/' ;
EQUAL : '=' ;
EQUAL_OP : '==' ;
CONCAT : '//' ;
NOT_EQUAL : '/=' ;
LT : '<' ;
LE : '<=' ;
GT : '>' ;
GE : '>=' ;
AND : '.and.' | 'AND' ;
OR : '.or.' | 'OR' ;
NOT : '.not.' | 'NOT' ;
EQV : '.eqv.' | 'EQV' ;
NEQV : '.neqv.' | 'NEQV' ;
EQ : '.eq.' | 'EQ' ;
NE : '.ne.' | 'NE' ;
LT_OP : '.lt.' ;
LE_OP : '.le.' ;
GT_OP : '.gt.' ;
GE_OP : '.ge.' ;
DOT : '.' ;
QUESTION : '?' ;
TRUE : 'TRUE' | '.true.' ;
FALSE : 'FALSE' | '.false.' ;
NULL : 'NULL' | 'null' ;
ZERO : 'ZERO' ;
ONE : 'ONE' ;

// Fortran BOZ literal: Z'1a0', B'1010', O'77' (single or double quotes).
BOZ
    : [ZzBbOo] ( '\'' ~['\r\n]* '\'' | '"' ~["\r\n]* '"' )
    ;

IDENTIFIER
    : [a-zA-Z_][a-zA-Z0-9_]*
    ;

INTEGER
    : [0-9]+
    ;

REAL_NUMBER
    : [0-9]+ '.' [0-9]* ([dDeE] [+-]? [0-9]+)?
    | '.' [0-9]+ ([dDeE] [+-]? [0-9]+)?
    | [0-9]+ [dD] [+-]? [0-9]+
    ;

// A doubled quote inside a string is an escaped quote (Fortran style):
// "xyz""'*" is one literal. A string may also span lines: a '&' near end of
// line continues onto the next, which may start with an optional '&'
// (`"abc&` <newline> `&def"`).
STRING
    : '"' (STR_CONT | '""' | ~["\r\n] | '\\' .)* '"'
    | '\'' (STR_CONT | '\'\'' | ~['\r\n] | '\\' .)* '\''
    ;

fragment STR_CONT
    : '&' [ \t]* '\r'? '\n' [ \t]* '&'?
    ;

// Comments and preprocessor lines go on the hidden channel: the parser ignores
// them (so they cannot disturb block structure) and the translator recovers
// them by token position.
// A comment line ending in '&' is a commented-out continuation line (common in
// long argument lists where some arguments are commented out). It is skipped —
// including its newline — so the continuation chain is not broken. Must precede
// COMMENT and NEWLINE.
COMMENT_CONT
    : '!' ~[\r\n]* '&' [ \t]* '\r'? '\n' -> skip
    ;

COMMENT
    : '!' ~[\r\n]* -> channel(HIDDEN)
    ;

PP_LINE
    : '#' ~[\r\n]* -> channel(HIDDEN)
    ;

// Line continuation: a line ending in '&' is joined with the next, which may
// itself begin with an optional leading '&'. Discarded so the logical line is
// presented to the parser as one. Must precede NEWLINE.
CONTINUATION
    : '&' [ \t]* ('!' ~[\r\n]*)? '\r'? '\n' [ \t]* ('&' [ \t]*)? -> skip
    ;

NEWLINE
    : '\r'? '\n'
    ;

WS
    : [ \t]+ -> skip
    ;
