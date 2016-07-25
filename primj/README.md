# Module: Primj

This language module builds on top of calcj and introduces functions,
variables, block statements, for loop, while loop, do-while loop, if-else
statements, ternary (conditional) expressions, return statements, assign
statements and function application. This is the first language which
introduces mutability and defines and uses names. Names are defined by both
MethodDef and ValDef and used by Ident and TypeUse. AST types in this language
module that can define a new symbol are: MethodDef, ValDef, Block and For. The
reason Block and For define their own symbols is because they define new
lexical scopes.

This phase introduces symbol assigner phase family to assign symbols to trees
that define new symbols and assign lexical owners to every trees. Then the
namer phase runs and binds the uses to definitions. Then the typer phase runs
performs type-checking. The last phase is shape checking to validate the shape
of the tree. For example, shape checking rules out a parameter that have
right-hand side.


