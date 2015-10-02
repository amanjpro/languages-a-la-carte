/*
 [The "BSD licence"]
 Copyright (c) 2013 Terence Parr, Sam Harwell
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

grammar Primj;

// starting point for parsing a primj file
program
    :   defDecleration* EOF
    ;

defDecleration
    : methodDeclaration
    | variableDeclaration ';'
    ;

methodDeclaration
    :   type Identifier formalParameters methodBody
    ;


variableDeclaration
    : mods='final'? type Identifier varRHS (',' Identifier varRHS)*
    ;

varRHS
    : ('=' expression)?
    ;

type
    :   primitiveType
    |   voidType
    ;

primitiveType
    :   'boolean'
    |   'char'
    |   'byte'
    |   'short'
    |   'int'
    |   'long'
    |   'float'
    |   'double'
    ;

voidType
    : 'void'
    ;



formalParameters
    :   '(' formalParameterList? ')'
    ;

formalParameterList
    :   formalParameter (',' formalParameter)* 
    ;

formalParameter
    :   mods='final'? type Identifier
    ;


methodBody
    :   block
    ;

literal                                                
    :   IntegerLiteral                                                # IntLit
    |   FloatingPointLiteral                                          # FloatLit
    |   CharacterLiteral                                              # CharLit
    |   StringLiteral                                                 # StrLit
    |   BooleanLiteral                                                # BoolLit
    ;


// STATEMENTS / BLOCKS

block
    :   '{' statement* '}'
    ;

statement
    :   block                                                           # BlockStmt
    |   variableDeclaration ';'                                         # VarStmt
    |   'if' parExpression statement ('else' statement)?                # If
    |   'for' '(' forControl ')' statement                              # For
    |   'while' parExpression statement                                 # While
    |   'do' statement 'while' parExpression ';'                        # DoWhile
    |   'return' expression? ';'                                        # Return
    |   assign ';'                                                      # AssignStmt
    |   ';'                                                             # Empty
    |   expression ';'                                                  # ExprStmt
    ;

assign
    : <assoc=right> Identifier op=('+='
                                  |'-='
                                  |'*='
                                  |'/='
                                  |'&='
                                  |'|='
                                  |'^='
                                  |'%='
                                  |'<<='
                                  |'>>='
                                  |'>>>='
                                  |'=') expression
    ;



forControl
    :   forInit? ';' expression? ';' forUpdate?
    ;

variableDefinition
    : mods='final'? type Identifier '=' 
              expression (',' Identifier '=' expression)*
    ;


forInit
    :   variableDefinition
    |   expressionList
    ;


forUpdate
    :   expressionList
    ;

// EXPRESSIONS

parExpression
    :   '(' expression ')'
    ;

expressionList
    :   expression (',' expression)*
    |   expression
    ;

expression
    :   Identifier arguments                                             # Apply
    |   primary                                                          # Primaries
    |   op=('+'|'-'|'++'|'--') expression                                # UnaryNum
    |   op=('~'|'!') expression                                          # UnaryElse
    |   expression op=('++' | '--')                                      # Postfix
    |   parExpression '?' expression ':' expression                      # Ternary
    |   expression op=('*'|'/'|'%') expression                           # Mul
    |   expression op=('+'|'-') expression                               # Add
    |   expression op=('<<' | '>>>' | '>>') expression                   # Shifts
    |   expression op=('<=' | '>=' | '>' | '<') expression               # Rel  
    |   expression op=('==' | '!=') expression                           # Equ 
    |   expression '&' expression                                        # BAnd
    |   expression '^' expression                                        # BXor
    |   expression '|' expression                                        # BOr
    |   expression '&&' expression                                       # And
    |   expression '||' expression                                       # Or
    |   '(' primitiveType ')' expression                                 # Cast
    ;

primary
    :   parExpression                                                    # Paren
    |   literal                                                          # Lit
    |   Identifier                                                       # Id
    ;

arguments
    :   '(' expressionList? ')'
    ;

// LEXER

// §3.9 Keywords

BOOLEAN       : 'boolean';
BYTE          : 'byte';
CHAR          : 'char';
DO            : 'do';
DOUBLE        : 'double';
ELSE          : 'else';
FLOAT         : 'float';
FOR           : 'for';
IF            : 'if';
INT           : 'int';
LONG          : 'long';
RETURN        : 'return';
SHORT         : 'short';
WHILE         : 'while';
VOID          : 'void';
FINAL         : 'final';

// §3.10.1 Integer Literals

IntegerLiteral
    :   DecimalIntegerLiteral
    |   HexIntegerLiteral
    |   OctalIntegerLiteral
    |   BinaryIntegerLiteral
    ;

fragment
DecimalIntegerLiteral
    :   DecimalNumeral IntegerTypeSuffix?
    ;

fragment
HexIntegerLiteral
    :   HexNumeral IntegerTypeSuffix?
    ;

fragment
OctalIntegerLiteral
    :   OctalNumeral IntegerTypeSuffix?
    ;

fragment
BinaryIntegerLiteral
    :   BinaryNumeral IntegerTypeSuffix?
    ;

fragment
IntegerTypeSuffix
    :   [lL]
    ;

fragment
DecimalNumeral
    :   '0'
    |   NonZeroDigit (Digits? | Underscores Digits)
    ;

fragment
Digits
    :   Digit (DigitOrUnderscore* Digit)?
    ;

fragment
Digit
    :   '0'
    |   NonZeroDigit
    ;

fragment
NonZeroDigit
    :   [1-9]
    ;

fragment
DigitOrUnderscore
    :   Digit
    |   '_'
    ;

fragment
Underscores
    :   '_'+
    ;

fragment
HexNumeral
    :   '0' [xX] HexDigits
    ;

fragment
HexDigits
    :   HexDigit (HexDigitOrUnderscore* HexDigit)?
    ;

fragment
HexDigit
    :   [0-9a-fA-F]
    ;

fragment
HexDigitOrUnderscore
    :   HexDigit
    |   '_'
    ;

fragment
OctalNumeral
    :   '0' Underscores? OctalDigits
    ;

fragment
OctalDigits
    :   OctalDigit (OctalDigitOrUnderscore* OctalDigit)?
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
OctalDigitOrUnderscore
    :   OctalDigit
    |   '_'
    ;

fragment
BinaryNumeral
    :   '0' [bB] BinaryDigits
    ;

fragment
BinaryDigits
    :   BinaryDigit (BinaryDigitOrUnderscore* BinaryDigit)?
    ;

fragment
BinaryDigit
    :   [01]
    ;

fragment
BinaryDigitOrUnderscore
    :   BinaryDigit
    |   '_'
    ;

// §3.10.2 Floating-Point Literals

FloatingPointLiteral
    :   DecimalFloatingPointLiteral
    |   HexadecimalFloatingPointLiteral
    ;

fragment
DecimalFloatingPointLiteral
    :   Digits '.' Digits? ExponentPart? FloatTypeSuffix?
    |   '.' Digits ExponentPart? FloatTypeSuffix?
    |   Digits ExponentPart FloatTypeSuffix?
    |   Digits FloatTypeSuffix
    ;

fragment
ExponentPart
    :   ExponentIndicator SignedInteger
    ;

fragment
ExponentIndicator
    :   [eE]
    ;

fragment
SignedInteger
    :   Sign? Digits
    ;

fragment
Sign
    :   [+-]
    ;

fragment
FloatTypeSuffix
    :   [fFdD]
    ;

fragment
HexadecimalFloatingPointLiteral
    :   HexSignificand BinaryExponent FloatTypeSuffix?
    ;

fragment
HexSignificand
    :   HexNumeral '.'?
    |   '0' [xX] HexDigits? '.' HexDigits
    ;

fragment
BinaryExponent
    :   BinaryExponentIndicator SignedInteger
    ;

fragment
BinaryExponentIndicator
    :   [pP]
    ;

// §3.10.3 Boolean Literals

BooleanLiteral
    :   'true'
    |   'false'
    ;

// §3.10.4 Character Literals

CharacterLiteral
    :   '\'' SingleCharacter '\''
    |   '\'' EscapeSequence '\''
    ;

fragment
SingleCharacter
    :   ~['\\]
    ;

// §3.10.5 String Literals

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
    |   OctalEscape
    |   UnicodeEscape
    ;

fragment
OctalEscape
    :   '\\' OctalDigit
    |   '\\' OctalDigit OctalDigit
    |   '\\' ZeroToThree OctalDigit OctalDigit
    ;

fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;

fragment
ZeroToThree
    :   [0-3]
    ;

// §3.10.7 The Null Literal

NullLiteral
    :   'null'
    ;

// §3.11 Separators

LPAREN          : '(';
RPAREN          : ')';
LBRACE          : '{';
RBRACE          : '}';
SEMI            : ';';
COMMA           : ',';

// §3.12 Operators

ASSIGN          : '=';
GT              : '>';
LT              : '<';
BANG            : '!';
TILDE           : '~';
QUESTION        : '?';
COLON           : ':';
EQUAL           : '==';
LE              : '<=';
GE              : '>=';
NOTEQUAL        : '!=';
AND             : '&&';
OR              : '||';
INC             : '++';
DEC             : '--';
ADD             : '+';
SUB             : '-';
MUL             : '*';
DIV             : '/';
BITAND          : '&';
BITOR           : '|';
CARET           : '^';
MOD             : '%';
ADD_ASSIGN      : '+=';
SUB_ASSIGN      : '-=';
MUL_ASSIGN      : '*=';
DIV_ASSIGN      : '/=';
AND_ASSIGN      : '&=';
OR_ASSIGN       : '|=';
XOR_ASSIGN      : '^=';
MOD_ASSIGN      : '%=';
LSHIFT_ASSIGN   : '<<=';
RSHIFT_ASSIGN   : '>>=';
URSHIFT_ASSIGN  : '>>>=';

// §3.8 Identifiers (must appear after all keywords in the grammar)

Identifier
    :   JavaLetter JavaLetterOrDigit*
    ;

fragment
JavaLetter
    :   [a-zA-Z$_] // these are the "java letters" below 0xFF
    |   // covers all characters above 0xFF which are not a surrogate
        ~[\u0000-\u00FF\uD800-\uDBFF]
        {Character.isJavaIdentifierStart(_input.LA(-1))}?
    |   // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
        [\uD800-\uDBFF] [\uDC00-\uDFFF]
        {Character.isJavaIdentifierStart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)))}?
    ;

fragment
JavaLetterOrDigit
    :   [a-zA-Z0-9$_] // these are the "java letters or digits" below 0xFF
    |   // covers all characters above 0xFF which are not a surrogate
        ~[\u0000-\u00FF\uD800-\uDBFF]
        {Character.isJavaIdentifierPart(_input.LA(-1))}?
    |   // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
        [\uD800-\uDBFF] [\uDC00-\uDFFF]
        {Character.isJavaIdentifierPart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)))}?
    ;

//
// Additional symbols not defined in the lexical specification
//

AT : '@';
ELLIPSIS : '...';

//
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
