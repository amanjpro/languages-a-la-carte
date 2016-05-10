

grammar Oberon0;


// PARSER



selector
  : ('.' Identifier | '[' expression ']')
  ;

number
  : Integer
  ;


booleanValue
  : value=('TRUE'| 'FALSE')
  ;

select
  : Identifier selector*
  ;

factor
  : select
  | number
  | booleanValue
  | '(' expression ')'
  | '~' factor
  ;

term
  : factor term2*
  ;

term2
  : op=('*' | 'DIV' | 'MOD' | '&') factor
  ;

simpleExpression
  : (sign=('+' | '-'))? term simpleExpression2*
  ;

simpleExpression2
  : op=('+' | '-' | 'OR') term
  ;

expression
  : simpleExpression (op=('=' | '#' | '<' | '<=' | '>' | '>=') simpleExpression)?
  ;

assignment
  : select ':=' expression
  ;


actualParameters
  : '(' (expression (',' expression)* )? ')'
  ;

procedureCall
  : select actualParameters?
  ;


ifStatement
  : 'IF' expression 'THEN' statementSequence elseIf* elsep
  ;

elseIf
  : 'ELSEIF' expression 'THEN' statementSequence
  ;

elsep
  : ('ELSE' statementSequence)? 'END'
  ;

whileStatement
  : 'WHILE' expression 'DO' statementSequence 'END'
  ;

statement
  : (assignment | procedureCall | ifStatement | whileStatement)?
  ;

statementSequence
  : statement (';' statement)*
  ;

identList
  : Identifier (',' Identifier)*
  ;

arrayType
  : 'ARRAY' expression 'OF' type
  ;

fieldList
  : (identList ':' type)?
  ;

recordType
  : 'RECORD' fieldList (';' fieldList)* 'END'
  ;

type
  : Identifier
  | arrayType
  | recordType
  ;

fpSection
  : 'VAR'? identList ':' type
  ;

formalParameters
  : '(' (fpSection (';' fpSection)* )? ')'
  ;

procedureHeading
  : 'PROCEDURE' name formalParameters?
  ;

procedureBody
  : declarations ('BEGIN' statementSequence)? 'END' name
  ;

procedureDeclaration
  : procedureHeading ';' procedureBody
  ;

declarations
  : constDeclaration? typeDeclaration? varDeclaration?
    (procedureDeclaration ';')*
  ;

constDeclaration
  : 'CONST' (Identifier '=' expression ';')*
  ;

typeDeclaration
  : 'TYPE' (Identifier '=' type ';')*
  ;

varDeclaration
  : 'VAR' (identList ':' type ';')*
  ;


name
  : Identifier
  ;

module
  : 'MODULE' name ';' declarations ('BEGIN' statementSequence)?
    'END' name '.'
  ;






// LEXER

// keywords

TRUE         :        'TRUE' ;
FALSE        :       'FALSE' ;
DIV          :         'DIV' ;
MOD          :         'MOD' ;
OR           :          'OR' ;
IF           :          'IF' ;
THEN         :        'THEN' ;
ELSEIF       :      'ELSEIF' ;
ELSE         :        'ELSE' ;
END          :         'END' ;
BEGIN        :       'BEGIN' ;
WHILE        :       'WHILE' ;
DO           :          'DO' ;
ARRAY        :       'ARRAY' ;
OF           :          'OF' ;
RECORD       :      'RECORD' ;
VAR          :         'VAR' ;
PROCEDURE    :   'PROCEDURE' ;
CONST        :       'CONST' ;
TYPE         :        'TYPE' ;
MODULE       :      'MODULE' ;


// Punctuations and operators

DOT           : '.'  ;
LBRACKET      : '['  ;
RBRACKET      : ']'  ;
LPAREN        : '('  ;
RPAREN        : ')'  ;
TILDA         : '~'  ;
MUL           : '*'  ;
AND           : '&'  ;
PLUS          : '+'  ;
MINUS         : '-'  ;
EQUAL         : '='  ;
SHARP         : '#'  ;
LT            : '<'  ;
LE            : '<=' ;
GT            : '>'  ;
GE            : '>=' ;
ASSIGN        : ':=' ;
COMA          : ','  ;
SEMI          : ';'  ;
COLON         : ':'  ;




Identifier
  : Letter (Letter | Digit)*
  ;


Integer
  : Digit+
  ;

Digit
  : [0-9]
  ;

Letter
  : [a-zA-Z_]
  ;

// Whitespace and comments
//

WS  :  [ \t\r\n\u000C]+ -> skip
    ;

COMMENT
    :   '/*' .*? '*/' -> skip
    ;

LINE_COMMENT
    :   '//' ~[\r\n]* -> skip
    ;
