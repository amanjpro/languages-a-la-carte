# Module: OOJ

This module builds on top of brokenj and introduces: classes, fields, methods,
constructors, qualified names, super and this references, new statements, null
references and packages, the concept of compilation units. This language in
this module is object-oriented and it has method overloading. The AST types
that define new symbols are: ClassDef, PackageDef, CompilationUnit, MethodDef,
ValDef, For, Block. The AST types that can use symbols are: Ident, Select,
TypeUse.

Compilation process in this module is more complicated than previous ones. It
also starts by symbol-assigner phase, which follows by namer phase. Then a new
phase is introduced called def-typer which assigns type to the top-level trees
like classes, fields, constructors and methods. Then a simple constant-folding
is performed, this phase is important to type-check the guards of case
statements.  At this point, the typer phase is executed which rules out invalid
programs.  Then, shape-checker, label-checker and jump-checker phases are
followed. Then, another check is performed to rule out forward referencing,
called forward-ref-checker, which is followed by constructor-checker to
validate the correctness of constructors. And finally, a flow-analyzer is run
to check against the usage of uninitialized variables and unreachable
statements.
