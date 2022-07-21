grammar project ;

/* LET'S START THE PROJECT! */
program: importStatement* line* EOF;
line: programStart | expression  | (ifBlock | forBlock | switchCase | whileBlock | doWhileBlock  | exception | objectInstantiation) ;
programStart:  defineConstants | class ;
/*____________________________________________________________________________________________________________________________________________________*/
/*  IMPORTING LIBRARIES */
importStatement: (importType1 | importType2 | importType3 | importType4) ;
importType1: (VALIDNAME '=' FROM VALIDNAME REQUIRE VALIDNAME ';');
importType2: (VALIDNAME '=' REQUIRE VALIDNAME ';');
importType3: (VALIDNAME '=' FROM VALIDNAME IMPORT VALIDNAME ';');
importType4: ((VALIDNAME) ((',' VALIDNAME)*) '=' ) (FROM VALIDNAME REQUIRE VALIDNAME)
             ((',' FROM VALIDNAME REQUIRE VALIDNAME)*';');
/*____________________________________________________________________________________________________________________________________________________*/
/* CLASS BODY */
class: CLASS VALIDNAME ('('VALIDNAME')')?  (IMPLEMENTS (VALIDNAME) (',' VALIDNAME)*)? BEGIN classImplementation END ;
classImplementation: (variableImplementation | constructor | function | objectInstantiation
                   | (ifBlock | forBlock | switchCase | whileBlock | doWhileBlock  | exception))* ;
constructor: VALIDNAME '(' functionParameter? (','functionParameter)* ')' BEGIN block END;
/*____________________________________________________________________________________________________________________________________________________*/
/* DEFINING FUNCTION DETAILS, AS A PART OF CLASS */
function: (ACCEESSIBILITY)? ('static' | 'void')? (INT | FLOAT | STRING | BOOL | CHAR | DOUBLE)? VALIDNAME '(' ((functionParameter) (','functionParameter)*)? ')' BEGIN block RETURN VALIDNAME ';' END
        | (ifBlock | forBlock | switchCase | whileBlock | doWhileBlock  | exception) ;
/*____________________________________________________________________________________________________________________________________________________*/
/* VARIABLE DEFINITION */
variableImplementation:  ( defineConstants  | arrayDefine );
arrayDefine: (arrayAssign1 | arrayAssign2 ) ';' ;
defineConstants: (defineConstantsType2 | defineConstantsType1  ) ;
defineConstantsType1: ((ACCEESSIBILITY)? (CONST)? newAssign ';');
defineConstantsType2: ((ACCEESSIBILITY)? (CONST)? (INT | FLOAT | STRING | BOOL | CHAR | DOUBLE) VALIDNAME  (',' VALIDNAME)* ';');
functionParameter: (INT | FLOAT | STRING | BOOL | CHAR | DOUBLE) VALIDNAME | newAssign;
/*____________________________________________________________________________________________________________________________________________________*/
/* DEFINE NEW OBJECTS */
objectInstantiation: ((ACCEESSIBILITY | CONST)? VALIDNAME VALIDNAME '=' VALIDNAME '(' ( ((INTEGER|FLOATING|'"'STRING'"')',')*(INTEGER|FLOATING|'"'STRING'"') )? ')' ';')
                     | ((ACCEESSIBILITY | CONST) VALIDNAME VALIDNAME ';')
                     | (VALIDNAME assigment ';') ;
/*____________________________________________________________________________________________________________________________________________________*/
/* HANDLING EXCEPTIONS */
exception: (TRY BEGIN block END CATCH '(' VALIDNAME (',' VALIDNAME)* ')' BEGIN block END);
/*____________________________________________________________________________________________________________________________________________________*/
/* DIFFERENT LOOPS AND BLOCKS AND CHECKING CONDITIONS */
ifBlock: IF '('expression')' BEGIN block END ((ELSE IF '(' expression')' BEGIN elseIfBlock END)* (ELSE BEGIN elseIfBlock END)* )?;
elseIfBlock: block | ifBlock;
switchCase: (SWITCH VALIDNAME BEGIN (caseBlock (BREAK ';')?)* defaultBlock? (BREAK ';')?  );
caseBlock: CASE ('"'VALIDNAME'"'|INTEGER) ':' block ;
defaultBlock: (DEFAULT ':' block );
whileBlock: WHILE '(' expression ')' BEGIN block END;
doWhileBlock: DOWHILE BEGIN block END WHILE '('expression')' ;
forBlock: (forBlockType1 | forBlockType2) ;
forBlockType1: (FOR '(' INT VALIDNAME '=' INTEGER ';' (expression) (('and'|'or') expression)* (';' VALIDNAME UNARYOPERATION )?')' BEGIN block END);
forBlockType2: (FOR VALIDNAME IN VALIDNAME BEGIN block END );
/*____________________________________________________________________________________________________________________________________________________*/
/* ONE LINE IF! */
ternaryExpression: VALIDNAME '=' VALIDNAME '?' ternaryExpressionStyle ':' ternaryExpressionStyle ';';
ternaryExpressionStyle:  '"' VALIDNAME '"' | INTEGER;
/*____________________________________________________________________________________________________________________________________________________*/
/* SIMPLE ASSIGN */
assigment: (VALIDNAME '=' expression) ;
/*____________________________________________________________________________________________________________________________________________________*/
/* ASIIGN, CONSIDERING DIFFERENT TYPES*/
newAssign: ((STRING)? VALIDNAME (('=') '"'VALIDNAME'"' (',' VALIDNAME '=' '"'VALIDNAME'"')*)?)
           | ((INT)? VALIDNAME (('=') (INTEGER | FLOATING)(',' VALIDNAME '=' (INTEGER|FLOATING))*)?)
           | ((FLOAT)? VALIDNAME (('=') FLOATING (',' VALIDNAME '=' FLOATING)*)?)
           | ((DOUBLE)? VALIDNAME (('=') FLOATING (',' VALIDNAME '=' FLOATING)*)?)
           | ((CHAR)? VALIDNAME (('=') ALPHABET (',' VALIDNAME '=' ALPHABET)*)?)
           | ((BOOL)? VALIDNAME (('=') BOOLIAN (',' VALIDNAME '=' BOOLIAN)*)?)
           ;
arrayAssign1: STRING VALIDNAME '[' ']' '=' 'new' STRING '[' INTEGER ']'
           | INT VALIDNAME '[' ']' '=' 'new' (INT|FLOAT) '[' INTEGER ']'
           | FLOAT VALIDNAME '[' ']' '=' 'new' FLOAT '[' INTEGER ']'
           | DOUBLE VALIDNAME '[' ']' '=' 'new' DOUBLE '[' INTEGER ']'
           | CHAR VALIDNAME '[' ']' '=' 'new' CHAR '[' INTEGER ']'
           | BOOL VALIDNAME '[' ']' '=' 'new' BOOL '[' INTEGER ']'
           ;
arrayAssign2: STRING VALIDNAME '[' ']' '=' '[' (VALIDNAME) (','(VALIDNAME))* ']'
            | INT VALIDNAME '[' ']' '=' '[' (INTEGER|FLOATING) (','(INTEGER|FLOATING))* ']'
            | FLOAT VALIDNAME '[' ']' '=' '[' (FLOATING) (','(FLOATING))* ']'
            | DOUBLE VALIDNAME '[' ']' '=' '[' (FLOATING) (','(FLOATING))* ']'
            | CHAR '\''VALIDNAME'\'' '[' ']' '=' '[' ('\''ALPHABET'\'') (','('\''ALPHABET'\''))* ']'
            | BOOL VALIDNAME '[' ']' '=' '[' (BOOLIAN) (','(BOOLIAN))* ']'
            ;
functionCall: VALIDNAME '(' (expression (',' expression)*)? ')' ';';
block: line* ;
/*____________________________________________________________________________________________________________________________________________________*/
/* DIFFERENT TYPES OF EXPRESSIONS USED IN CONDITIONS, BLOCKS AND ... */
expression
    : '(' expression ')'
    | expression POWEROPERARTIONS expression
    | expression NOTOPERATION expression
    /*| expression ('+'expression | '-'expression) expression*/
    | expression UNARYOPERATION expression
    | expression MULTIPLYOPERATIONS expression
    | expression PLUSORMINUSOPERATIONS expression
    | expression SHIFT expression
    | expression BITWISEOPERATIONS expression
    | expression EQUALITY expression
    | expression COMPARE expression
    | expression ('not' | (( 'and' | 'or' | '||' | '&&')expression))
    | expression ASSIGNEXPRESSIONS expression
    | functionCall
    | '!' expression
    | assigment ';'
    | CONSTANT (';')?
    | VALIDNAME(('.'VALIDNAME)?) (';')?
    | ALPHABET (';')?
    | INTEGER (';')?
    | FLOATING (';')?
    (';')?
    ;
/*____________________________________________________________________________________________________________________________________________________*/
/* LEXERS */
COMPARE: (LT | GT | GE | LE) ;
LT: '<';
GT: '>';
GE: '>=';
LE: '<=';
UNARYOPERATION: '++' | '--';
POWEROPERARTIONS: '**';
NOTOPERATION: '~';
MULTIPLYOPERATIONS: '*' | '/' | '//' |'%' ;
PLUSORMINUSOPERATIONS: '+' | '-' ;
BITWISEOPERATIONS: '&' | '^' ;
EQUALITY: ('==' | '!=' | '<>');
ASSIGNEXPRESSIONS: ('=' | '+=' | '-=' | '*=' | '/=');
COMPAREOPERATIONS: (EQUALITY | COMPARE);
SHIFT: ('<<' | '>>');
BOOL_OPERATOR: 'and' | 'or' | 'xor' ;
INTEGER: [0-9]+;
FLOATING: [0-9]+ '.' [0-9]+;
SCIENTIFIC_SYMBOL:  INTEGER+|(INTEGER('.'INTEGER+)?'e'('-'|'+')? INTEGER);
BOOLIAN: 'true' | 'false';
BOOL: 'bool';
CHAR: 'char';
ACCEESSIBILITY: ('public' | 'private' | 'protected') ;
NULL: 'Null';
FROM: 'from';
REQUIRE: 'require';
CONST: 'const';
STRING: 'string';
INT: 'int';
FLOAT: 'float';
DOUBLE: 'double';
CLASS: 'class';
IMPLEMENTS: 'implements';
BEGIN: 'begin';
END: 'end';
THIS: 'this';
FOR: 'for';
RETURN: 'return';
IN: 'in';
WHILE: 'while';
DOWHILE: 'do';
IF: 'if';
ELSE: 'else';
SWITCH: 'switch';
BREAK: 'break';
DEFAULT: 'default';
CASE: 'case';
TRY: 'try';
CATCH: 'catch';
IMPORT: '=>';
CONSTANT: (INT | FLOAT | STRING | BOOL | CHAR | DOUBLE | SCIENTIFIC_SYMBOL | NULL) ;
VALIDNAME:  (ALPHABET|'$')(ALPHABET|'_'|'.'|'$'|INTEGER)+ ;  //CONTROLL ON VARIABLE NAMES
ALPHABET: [a-zA-Z]+ ;
WS: [ \t\r\n]+ -> skip;   //SKIP BLANKS
SINGLELINECOMMENT: '//' (VALIDNAME | ' ')* -> skip;
MULTILINECOMMENT: '/*' (VALIDNAME|  INTEGER| ' '| ':' | '-' | '\n')* '*/' -> skip;
/*____________________________________________________________________________________________________________________________________________________*/