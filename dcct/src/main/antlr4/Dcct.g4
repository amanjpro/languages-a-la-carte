grammar Dcct;


// Parser

program
  : schema? actionDeclaration+ EOF
  | schema  actionDeclaration* EOF
  ;

///////////////////////// Schema and schema contents ///////////////////////

schema
  : cloudDataDecl+ 
  ;

// Types

// TODO just use generic identifier and rely on type checking to rule out wrong
// identifiers? Before we had entityIdent and arrayIdent now just ident. It
// seems to be more sensible for this to be a type error rather than a parse
// error.

indexType
  : 'Int'    
  | 'String'
  | Identifier // of array or entity
  ;

// a cloud type consists of: consistency annotation, setIdentifier, and a cloud
// type itself.
// TODO I do not provide an easy way create sets and stuff, but this works for
// now.

cloudType
  : annotationType? Identifier? cloudPrimType;

cloudPrimType
  : 'CInt' 
  | 'CString' 
  | cloudSetType  
  ;
  
expressionType
  : indexType
  | 'Set' '<' expressionType '>'
  | expressionType '->' expressionType
  ;


//TODO add regions and stuff
annotationType
  : '@SR'
  | '@MAV'
  | '@EC'
  ;


cloudSetType
  : 'CSet' '<' indexType '>'
  ;

cloudDataDecl
  : entityDecl
  | arrayDecl
  ;


// Entities
// TODO think if I need to make the body optional
entityDecl
  : 'entity' Identifier '(' elements ')' ('{' properties '}')?
  ;

//TODO I think this def and the properties one are inconsistent
elements
  :   element? (',' element)*
  ;

element
  : Identifier ':' indexType
  ;
  
properties
  : property ';' (property ';')*
  ;
  
property
  : Identifier ':' cloudType
  ; 
  
  
// Arrays
// An array must have properties
  arrayDecl
  : 'array' Identifier '[' elements ']' '{' properties '}'
  ;
  
  
///////////////////////// Actions and expressions ///////////////////////

actionDeclaration
    :   'action' Identifier elements ':' indexType block
    ;

expressions
  : expression ';' (expression ';')*
  ;

block
  : '{'expressions '}'
  ; 


// Expressions, expression, and related stuff
expressionArgs
  :   expression (',' expression)*
  ;

expression
  // Statements 
  :  'new' Identifier '(' expressionArgs ')'                            # newEntity            // Create a new entity
  |  'delete' expression                                                # deleteEntity         // Delete an entity
  |  Identifier '[' expressionArgs ']'                                  # arraySelector        // Array selector
  |  expression '(' expressionArgs ')'                                  # actionCall           // action call, or apply
  |  expression '.' Identifier                                          # entityArraySelect    // Expression select
  |  'all' Identifier                                                   # allEntitesOrArrayElem           // all entities 
  |  foreach                                                            # foreachLoop          // foreach loop
  |  'var' indexType '=' expression                                     # valDecl              // var declaration, not an expression but whatever
  |  'if' '(' expression ')' block 'else' block                         # branching            // if or else, not an expression

  // Expressions
  |  <assoc=right> Identifier '=' expression                            # assign
  |  '(' expression ')'                                                 # parExpr
  |  Identifier                                                         # identifier           // also not an expression 
  |  literals                                                           # literal              // String or integer literals
  |  op=('+'|'-') expression                                            # UnaryNum
  |  '!' expression                                                     # UnaryBool
  |  expression op=('*'|'/'|'%') expression                             # Mul
  |  expression op=('+'|'-') expression                                 # Add
  |  expression op=('<=' | '>=' | '>' | '<') expression                 # Rel
  |  expression op=('==' | '!=') expression                             # Equ
  |  expression '&&' expression                                         # And
  |  expression '||' expression                                         # Or
  ;


foreach
  : 'foreach' Identifier 'in' ('all' | 'entries') expression '.' expression
    ('where' expression)?
    ('orderby' expression '.' expression)?
    block
  ;

literals
  : IntegerLiteral
  | StringLiteral
  ;
 

// LEXER
// TODO I think I need boolens as well
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
