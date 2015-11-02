/*
Copyright (c) <2015>, Amanj Sherwany, Terence Parr, Sam Harwell
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/** A Java 1.0 grammar for ANTLR v4, it is taken from ``The Java Language
 *  Specification 1.0'', and adapted to work with ANTLR v4.
 *
 *  The LEXER part is almost completely taken from Java 7's lexer by Terence
 *  Parr and Sam Harwell.
 *
 *  The uses ANTLR v4's left-recursive expression notation.
 *  It parses all standard Java 1.0 class, and also TTY
 *  class from sun.tools.ttydebug pacakge.
 *
 *  You can test with
 *
 *  $ antlr4 Java1.g4
 *  $ javac *.java
 *  $ grun Java1 compilationUnit *.java
 */


grammar Java1;

// 19.2 Starting point
compilationUnit
  : packageDeclaration? importDeclaration* typeDeclaration* EOF
  ;

// 19.3 Literals
literal
  : IntegerLiteral                                     # IntLit
  | FloatingPointLiteral                               # DoubleLit
  | BooleanLiteral                                     # BoolLit
  | CharacterLiteral                                   # CharLit
  | StringLiteral                                      # StringLit
  | NullLiteral                                        # NullLit
  ;

// 19.4 Types

type
  : primitiveType
  | referenceType
  ;

primitiveType
  : 'byte'
  | 'short'
  | 'int'
  | 'long'
  | 'char'
  | 'float'
  | 'double'
  | 'boolean'
  ;

referenceType
  : classOrInterfaceType
  | arrayType
  ;

classOrInterfaceType
  : name
  ;


arrayType
  : primitiveType '[' ']'
  | name '[' ']'
  | arrayType '[' ']'
  ;

// 19.5 Names
name
  : Identifier ('.' Identifier)*
  ;


packageDeclaration
  : 'package' Identifier ('.' Identifier)* ';'
  ;

importDeclaration
  : singleTypeImportDeclaration
  | typeImportOnDemandDeclaration
  ;

singleTypeImportDeclaration
  : 'import' name ';'
  ;

typeImportOnDemandDeclaration
  : 'import' name '.' '*' ';'
  ;

typeDeclaration
  : classDeclaration
  | interfaceDeclaration
  ;

// 19.7 LALR(1) Productions

modifier
  : 'public'
  | 'protected'
  | 'private'
  | 'static'
  | 'abstract'
  | 'final'
  | 'native'
  | 'synchronized'
  | 'transient'
  | 'volatile'
  ;

// 19.8 Classes
// 19.8.1 Class Declaration
classDeclaration
  : modifier* 'class' Identifier parent? interfaces? classBody
  ;

parent
  : 'extends' classOrInterfaceType
  ;

interfaces
  : 'implements' classOrInterfaceTypeList
  ;

classBody
  : '{' classBodyDeclaration* '}'
  ;

classBodyDeclaration
  : classMemberDeclaration
  | staticInitializer
  | constructorDeclaration
  ;

classMemberDeclaration
  : fieldDeclaration
  | methodDeclaration
  ;

// 19.8.2 Field Declaration
fieldDeclaration
  : modifier* type variableDeclarators ';'
  ;

variableDeclarators
  : variableDeclarator (',' variableDeclarator)*
  ;

variableDeclarator
  : variableDeclaratorId ('=' variableInitializer)?
  ;

variableDeclaratorId
  : Identifier dims?
  ;

variableInitializer
  : expression
  | arrayInitializer
  ;

// 19.8.3 Methods
methodDeclaration
  : methodHeader methodBody
  ;

methodHeader
  : modifier* type methodDeclarator throwsClause?           # TypedMethodHeader
  | modifier* 'void' methodDeclaratorNoDims throwsClause?   # VoidMethodHeader
  ;

methodDeclarator
  : methodDeclaratorNoDims dims?
  ;


methodDeclaratorNoDims
  : Identifier '(' formalParameterList? ')'
  ;

formalParameterList
  : formalParameter (',' formalParameter)*
  ;

formalParameter
  : type variableDeclaratorId
  ;

throwsClause
  : 'throws' classOrInterfaceTypeList
  ;

classOrInterfaceTypeList
  : classOrInterfaceType (',' classOrInterfaceType)*
  ;


methodBody
  : block
  | emptyStatement
  ;

// 19.8.4 Static Initializers
staticInitializer
  : 'static' block
  ;

// 19.8.5 Constructor Declaration
constructorDeclaration
  : modifier* constructorDeclarator throwsClause? constructorBody
  ;

constructorDeclarator
  : methodDeclaratorNoDims
  ;

constructorBody
  : '{' explicitConstructorInvocation? blockStatement* '}'
  ;

explicitConstructorInvocation
  : qual=('this' | 'super') '(' argumentList? ')' ';'
  ;

// 19.9 Interfaces
interfaceDeclaration
	: modifier* 'interface' Identifier extendsInterfaces? interfaceBody
  ;

extendsInterfaces
	: 'extends' classOrInterfaceTypeList
  ;

interfaceBody
	: '{' interfaceMemberDeclaration* '}'
  ;

interfaceMemberDeclaration
	: constantDeclaration
	| abstractMethodDeclaration
  ;

constantDeclaration
	: fieldDeclaration
  ;

abstractMethodDeclaration
	: methodHeader ';'
  ;


// 19.10 Productions from §10: Arrays

arrayInitializer
	: '{' variableInitializers? ','? '}'
  ;

variableInitializers
	: variableInitializer (',' variableInitializer)*
  ;

// 19.11 Productions from §14: Blocks and Statements

block
	: '{' blockStatement* '}'
  ;

blockStatement
	: localVariableDeclarationStatement
	| statement
  ;

localVariableDeclarationStatement
	: localVariableDeclaration ';'
  ;

localVariableDeclaration
	: type variableDeclarators
  ;

statement
	: statementWithoutTrailingSubstatement
	| labeledStatement
	| ifThenStatement
	| ifThenElseStatement
	| whileStatement
	| forStatement
  ;

statementNoShortIf
	: statementWithoutTrailingSubstatement
	| labeledStatementNoShortIf
	| ifThenElseStatementNoShortIf
	| whileStatementNoShortIf
	| forStatementNoShortIf
  ;

statementWithoutTrailingSubstatement
	: block
	| emptyStatement
	| expressionStatement
	| switchStatement
	| doStatement
	| breakStatement
	| continueStatement
	| returnStatement
	| synchronizedStatement
	| throwStatement
	| tryStatement
  ;

emptyStatement
	: ';'
  ;

labeledStatement
	: Identifier ':' statement
  ;

labeledStatementNoShortIf
	: Identifier ':' statementNoShortIf
  ;

expressionStatement
	: statementExpression ';'
  ;

statementExpression
	: expression
  ;

ifThenStatement
	: 'if' '(' expression ')' statement
  ;

ifThenElseStatement
	: 'if' '(' expression ')' statementNoShortIf 'else' statement
  ;

ifThenElseStatementNoShortIf
	: 'if' '(' expression ')' statementNoShortIf 'else' statementNoShortIf
  ;

switchStatement
	: 'switch' '(' expression ')' switchBlock
  ;

switchBlock
	: '{' switchBlockStatementGroups? switchLabel* '}'
  ;

switchBlockStatementGroups
	: switchBlockStatementGroup+
  ;

switchBlockStatementGroup
	: switchLabel+ blockStatement*
  ;

switchLabel
	: caseLabel
  | defaultCase
  ;

caseLabel
	: 'case' constantExpression ':'
  ;

defaultCase
  : 'default' ':'
  ;

whileStatement
	: 'while' '(' expression ')' statement
  ;

whileStatementNoShortIf
	: 'while' '(' expression ')' statementNoShortIf
  ;

doStatement
	: 'do' statement 'while' '(' expression ')' ';'
  ;

forStatement
	: 'for' '(' forInit? ';' expression? ';' forUpdate? ')' statement
  ;

forStatementNoShortIf
	: 'for' '(' forInit? ';' expression? ';' forUpdate? ')'
     statementNoShortIf
  ;

forInit
	: statementExpressionList
	| localVariableDeclaration
  ;

forUpdate
	: statementExpressionList
  ;

statementExpressionList
	: statementExpression (',' statementExpression)*
  ;

breakStatement
	: 'break' Identifier? ';'
  ;

continueStatement
	: 'continue' Identifier? ';'
  ;

returnStatement
	: 'return' expression? ';'
  ;

throwStatement
	: 'throw' expression ';'
  ;

synchronizedStatement
	: 'synchronized' '(' expression ')' block
  ;

tryStatement
	: 'try' block catches
	| 'try' block catches? finallyClause
  ;

catches
	: catchClause+
  ;

catchClause
	: 'catch' '(' formalParameter ')' block
  ;

finallyClause
	: 'finally' block
  ;


// 19.12 Productions from §15: Expressions

primary
  : primaryNoNewArray
  | arrayCreationExpression
  ;

arrayAccess
	: name '[' expression ']'
	| primaryNoNewArray '[' expression ']'
  ;

primaryNoNewArray
	: literal                                                # PrimaryLit
	| 'this'                                                 # PrimaryThis
	| '(' expression ')'                                     # PrimaryExpr
	| classInstanceCreationExpression                        # PrimaryNew
	| arrayCreationExpression '.' Identifier                 # PrimaryNewArray
	| primaryNoNewArray '.' Identifier                       # PrimarySelect
	| 'super' '.' Identifier                                 # PrimarySuperSelect
	| name '(' argumentList? ')'                             # PrimaryApply
	| primaryNoNewArray '.' Identifier '(' argumentList? ')' # PrimaryQualApply
	| arrayCreationExpression '.' Identifier
      '(' argumentList? ')'                                # PrimaryArrayApply
	| 'super' '.' Identifier '(' argumentList? ')'           # PrimarySuperApply
	| name '[' expression ']'                                # PrimaryArrayAccess
	| primaryNoNewArray '[' expression ']'                   # PrimaryArrayAccess2
  ;

classInstanceCreationExpression
	: 'new' classOrInterfaceType '(' argumentList? ')'
  ;

argumentList
	: expression (',' expression)*
  ;

arrayCreationExpression
	: 'new' primitiveType dimExpr+ dims?
	| 'new' classOrInterfaceType dimExpr+ dims?
  ;

dimExpr
	: '[' expression ']'
  ;

dims
	: dim+
  ;

dim
  : '[' ']'
  ;

fieldAccess
	: primary '.' Identifier                      # QualifiedFieldAccess
	| 'super' '.' Identifier                      # SuperFieldAccess
  ;

methodInvocation
	: name '(' argumentList? ')'                   # SimpleMethodInvocation
	| primary '.' Identifier '(' argumentList? ')' # QualifiedMethodInvocation
	| 'super' '.' Identifier '(' argumentList? ')' # SuperMethodInvocation
  ;


assignment
	: leftHandSide op=('='
                   | '*='
                   | '/='
                   | '%='
                   | '+='
                   | '-='
                   | '>>='
                   | '<<='
                   | '>>>='
                   | '&='
                   | '|='
                   | '^=') expression
  ;

leftHandSide
	: name
	| fieldAccess
	| arrayAccess
  ;


expression
  : primary                                            # PrimaryExpression
  | name                                               # NameExpression
	| '(' type ')' expression                            # CastExpression
  | expression op=('++' | '--')                        # PostfixExpression
  | op=('++' | '--' | '-' | '+') expression            # UnaryExpression
  | op=('~' | '!') expression                          # BitwiseUnaryExpression
  | expression op=('*' | '/' | '%') expression         # MulBinaryExpression
	| expression op=('+' | '-') expression               # AddBinaryExpression
	| expression op=('>>' | '<<' | '>>>') expression     # ShiftBinaryExpression
	| expression op=('<' | '>' | '<=' | '>=') expression # RelBinaryExpression
	| expression 'instanceof' type                       # InstanceOfExpression
	| expression op=('==' | '!=') expression             # EquBinaryExpression
	| expression '&' expression                          # BitAndBinaryExpression
	| expression '|' expression                          # BitOrBinaryExpression
	| expression '^' expression                          # BitXOrBinaryExpression
	| expression '&&' expression                         # AndBinaryExpression
	| expression '||' expression                         # OrBinaryExpression
	| expression '?' expression ':' expression           # TernaryExpression
  | leftHandSide
    op=('='  | '*='  | '/='  | '%='   | '+=' |
     '-=' | '<<=' | '>>=' | '>>>=' |
     '&=' | '^='  | '|=') expression                   # AssignExpression
  ;

constantExpression
	: expression
  ;

// LEXER

// §3.9 Keywords

ABSTRACT      : 'abstract';
BOOLEAN       : 'boolean';
BREAK         : 'break';
BYTE          : 'byte';
CASE          : 'case';
CATCH         : 'catch';
CHAR          : 'char';
CLASS         : 'class';
CONST         : 'const';
CONTINUE      : 'continue';
DEFAULT       : 'default';
DO            : 'do';
DOUBLE        : 'double';
ELSE          : 'else';
EXTENDS       : 'extends';
FINAL         : 'final';
FINALLY       : 'finally';
FLOAT         : 'float';
FOR           : 'for';
GOTO          : 'goto';
IF            : 'if';
IMPLEMENTS    : 'implements';
IMPORT        : 'import';
INSTANCEOF    : 'instanceof';
INT           : 'int';
INTERFACE     : 'interface';
LONG          : 'long';
NATIVE        : 'native';
NEW           : 'new';
PACKAGE       : 'package';
PRIVATE       : 'private';
PROTECTED     : 'protected';
PUBLIC        : 'public';
RETURN        : 'return';
SHORT         : 'short';
STATIC        : 'static';
SUPER         : 'super';
SWITCH        : 'switch';
SYNCHRONIZED  : 'synchronized';
THIS          : 'this';
THROW         : 'throw';
THROWS        : 'throws';
TRANSIENT     : 'transient';
TRY           : 'try';
VOID          : 'void';
VOLATILE      : 'volatile';
WHILE         : 'while';

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
    |   NonZeroDigit Digits?
    ;

fragment
Digits
    :   Digit+
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
HexNumeral
    :   '0' [xX] HexDigits
    ;

fragment
HexDigits
    :   HexDigit+
    ;

fragment
HexDigit
    :   [0-9a-fA-F]
    ;

fragment
OctalNumeral
    :   '0' OctalDigit+
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
BinaryNumeral
    :   '0' [bB] BinaryDigit+
    ;

fragment
BinaryDigit
    :   [01]
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
LBRACK          : '[';
RBRACK          : ']';
SEMI            : ';';
COMMA           : ',';
DOT             : '.';

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
