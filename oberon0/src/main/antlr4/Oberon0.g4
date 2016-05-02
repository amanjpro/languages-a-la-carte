

grammar Oberon0;


// PARSER



selector
  : ('.' Identifier | '[' expression ']')*
  ;

number
  : Integer
  ;


factor
  : Identifier selector
  | number
  | '(' expression ')'
  | '~' factor
  ;

term
  : factor (op=('*' | 'DIV' | 'MOD' | '&') factor)*
  ;

simpleExpression
  : (sign=('+' | '-'))? term (op=('+' | '-' | 'OR') term)*
  ;

expression
  : simpleExpression (op=('=' | '#' | '<' | '<=' | '>' | '>=') simpleExpression)?
  ;

assignment
  : Identifier selector ':=' expression
  ;


actualParameters
  : '(' (expression (',' expression)* )? ')'
  ;

procedureCall
  : Identifier selector (actualParameters)?
  ;


ifStatement
  : 'IF' expression 'THEN' statementSequence
     ('ELSEIF' expression 'THEN' statementSequence)*
     ('ELSE' statementSequence)? 'END'
  ;

whileStatement
  : 'WHILE' expression 'DO' statementSequence 'END'
  ;

statement
  : (assignment | procedureCall | ifStatement | whileStatement)?
  ;

statementSequence
  : statement (';' statement)
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
  : 'RECORD' fieldList (';' fieldList) 'END'
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
  : 'PROCEDURE' Identifier formalParameters?
  ;

procedureBody
  : declarations ('BEGIN' statementSequence)? 'END' Identifier
  ;

procedureDeclaration
  : procedureHeading ';' procedureBody
  ;

declarations
  : ('CONST' (Identifier '=' expression ';')* )?
    ('TYPE' (Identifier '=' type ';')* )?
    ('VAR' (identList ':' type ';')* )?
    (procedureDeclaration ';')*
  ;

module
  : 'MODULE' Identifier ';' declarations ('BEGIN' statementSequence)?
    'END' Identifier '.'
  ;






// LEXER

// keywords

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
