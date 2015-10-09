package ch.usi.inf.l3.sana.brokenj.namers

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast._
import tiny.errors.ErrorReporting.{error,warning}
import tiny.symbols._
import calcj.ast._
import primj.ast._
import primj.symbols._
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._
import primj.namers.SymbolAssignerComponent
import brokenj.ast._

/*
Done in Primj
Program: DONE
Assign: DONE
If: DONE
While: DONE
Block: DONE
For: DONE
Ternary: DONE
Apply: DONE
Return: DONE
MethodDef: DONE
ValDef: DONE
Ident: DONE
NoTree: DONE
TypeUse: DONE
Cast: DONE
Binary: DONE
Literal: DONE
Unary: DONE
*/

/*
Case: DONE
Swtich: DONE
Label: DONE
Break: DONE
Continue: DONE
*/


@component(tree, owner)
trait CaseSymbolAssigner extends SymbolAssignerComponent {
  (cse: Case)     => {
    val guards = cse.guards.map { guard =>
      assign((guard, owner)).asInstanceOf[Expr]
    }
    val body   = assign((cse.body, owner))
    cse.copy(guards = guards, body = body, owner = owner)
  }
}


@component(tree, owner)
trait SwitchSymbolAssigner extends SymbolAssignerComponent {
  (switch: Switch)     => {
    val expr  = assign((switch.expr, owner)).asInstanceOf[Expr]
    val cases = switch.cases.map { guard =>
      assign((guard, owner)).asInstanceOf[CaseApi]
    }
    switch.copy(cases = cases, expr = expr, owner = owner)
  }
}


@component(tree, owner)
trait LabelSymbolAssigner extends SymbolAssignerComponent {
  (label: Label)     => {
    val stmt  = assign((label.stmt, owner)).asInstanceOf[Expr]
    label.copy(stmt = stmt, owner = owner)
  }
}

@component(tree, owner)
trait BreakSymbolAssigner extends SymbolAssignerComponent {
  (break: Break)     => {
    break.copy(owner = owner)
  }
}

@component(tree, owner)
trait ContinueSymbolAssigner extends SymbolAssignerComponent {
  (continue: Continue)     => {
    continue.copy(owner = owner)
  }
}
