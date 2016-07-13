# Module: Calcj

This language module builds on top of primj and introduces switch statements,
break statements, continue statements and labeled statements.

This module has all the compilation phases that have been introduced by primj
and adds two more checks that are meant to run after shape-checking. The first
added phase is label-checker to check the validity of labeled statements. The
other added phase is jump-checker which checks the validity of continue and
break statements.
