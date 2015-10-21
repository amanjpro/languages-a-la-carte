grammar Dcct;


// Parser

program
  : schema ';' expression EOF
  ;

schema
  : decls
  ;

// Types
indexType
  : 'Int'
  | 'String'
  | entityIdentifier
  | arrayIdentifier
  ;

cloudType
  : 'CInt'
  | 'CString'
  | cloudSetType
  | cloudTypeIdentifier
  ;

cloudSetType
  : 'CSet' '<' indexType '>'
  ;

expressionType
  : indexType
  | 'Set' '<' expressionType '>'
  | expressionType '->' expressionType
  | tupleType
  ;

tupleType
  : '(' tupleElemType ')'
  ;

tupleElemType
  : expressionType
  | expressionType ',' tupleElemType
  ;

decl
  : entityDecl
  | arrayDecl
  | propertyDecl
  ;

entityDecl
  : 'entity' entityIdentifier '(' elements ')'
  ;

arrayDecl
  : 'array' arrayIdentifier '[' elements ']'
  ;

propertyDecl
  : 'property' Identifier ':' indexType '->' cloudType
  ;


elements
  : element ',' elements
  | element
  ;

element
  : Identifier ':' indexType
  ;

// Identifiers
entityIdentifier
  : Identifier
  ;

arrayIdentifier
  : Identifier
  ;

cloudTypeIdentifier
  : Identifier
  ;


expression
  : 'new' entityIdentifier '(' expressions ')'
  | 'delete' expression
  | arrayIdentifier '[' expressions ']'
  | expression '(' expressions ')'
  | expression '.' expression
  | 'all' entityIdentifier
  | 'entries' Identifier
  | 'yield'
  | 'flush'
  | expression expression
  | expression ';' expression
  | '(' expressions ')'
  | expression bop expression
  | foreach
  | value
  | varDeclaration
  ;


bop
  : '=='
  | '!='
  ;

varDeclaration
  : 'var' Identifier '=' value
  ;


value
  : Identifier
  | literals
  | Identifier '[' values ']'
  | '(' values ')'
  | '(' Identifier ':' expressionType ')' '=>' expression
  ;

decls
  : decl
  | decl ';' decls
  ;


ifelse
  : 'if' '(' expression ')' block 'else' block
  ;

block
  : '{' expression '}'
  ;


expressions
  : expression ',' expressions
  | expression
  ;

values
  : value ',' value
  | value
  ;

foreach
  : 'foreach' Identifier 'in' ('all' | 'entries') expression '.' expression
    ('where' expression bop expression)?
    ('orderby' expression '.' expression)?
    block
  ;

literals
  : IntegerLiteral
  | StringLiteral
  ;

// LEXER

// Types
INT        : 'Int';
STRING     : 'String';
SET        : 'Set';
CINT       : 'CInt';
CSTRING    : 'CString';
CSET       : 'CSet';


// Keywords

NEW        : 'new';
DELETE     : 'delete';
ALL        : 'all';
ENTRIES    : 'entries';
YIELD      : 'yield';
FLUSH      : 'flush';
PROPERTY   : 'property';
IF         : 'if';
ELSE       : 'else';
FOREACH    : 'foreach';
WHERE      : 'where';
VAR        : 'var';
ORDERBY    : 'orderby';
IN         : 'in';



// Separators
LPAREN     : '(';
RPAREN     : ')';
LBRACK     : '[';
RBRACK     : ']';
LBRACE     : '{';
RBRACE     : '}';
SEMI       : ';';
COMA       : ',';
DOT        : '.';



// Operators
LT         : '<';
GT         : '>';
COLON      : ':';
ARROW      : '->';
TARROW     : '=>';
ASSIGN     : '=';
BANG       : '!';


Identifier
  :   Letter LetterOrDigit*
  ;

fragment
Letter
  :   [a-zA-Z$_] // these are the "valid letters" below 0xFF
  |   // covers all characters above 0xFF which are not a surrogate
      ~[\u0000-\u00FF\uD800-\uDBFF]
      {Character.isJavaIdentifierStart(_input.LA(-1))}?
  |   // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
      [\uD800-\uDBFF] [\uDC00-\uDFFF]
      {Character.isJavaIdentifierStart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)))}?
  ;

fragment
LetterOrDigit
  :   [a-zA-Z0-9$_] // these are the "valid letters or digits" below 0xFF
  |   // covers all characters above 0xFF which are not a surrogate
      ~[\u0000-\u00FF\uD800-\uDBFF]
      {Character.isJavaIdentifierPart(_input.LA(-1))}?
  |   // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
      [\uD800-\uDBFF] [\uDC00-\uDFFF]
      {Character.isJavaIdentifierPart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)))}?
  ;

fragment
Digit: [0-9];

Digits: Digit+;

IntegerLiteral: Digits;

StringLiteral
  :   '"' StringCharacters? '"'
  ;

fragment
StringCharacters
  :   StringCharacter+
  ;

fragment
StringCharacter
  :   ~["\\]
  |   EscapeSequence
  ;

// §3.10.6 Escape Sequences for Character and String Literals

fragment
EscapeSequence
  :   '\\' [btnfr"'\\]
  ;




//
// Whitespace and comments
//

WS
  :  [ \t\r\n\u000C]+ -> skip
  ;

COMMENT
  :   '/*' .*? '*/' -> skip
  ;

LINE_COMMENT
  :   '//' ~[\r\n]* -> skip
  ;
