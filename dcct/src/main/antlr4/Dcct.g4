grammar Dcct;


// Parser

program
  : schema ';'EOF
  ;

schema
  : decls
  ;

// Types
indexType
  : 'Int'
  | 'String'
  | entityIdentifier
  ;

decls
  : decl
  | decl ';' decls
  ;

decl
  : entityDecl
  ;

entityDecl
  : 'entity' entityIdentifier '(' elements ')'
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
