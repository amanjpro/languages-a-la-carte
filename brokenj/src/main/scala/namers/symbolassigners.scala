package ch.usi.inf.l3.sana.brokenj.namers

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.ast.Implicits._
import tiny.errors.ErrorReporting.{error,warning}
import tiny.symbols._
import calcj.ast.{TreeCopiers => _, _}
import primj.ast.{TreeCopiers => _, _}
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
  (cse: CaseApi)     => {
    val guards = cse.guards.map { guard =>
      assign((guard, owner)).asInstanceOf[Expr]
    }
    val body   = assign((cse.body, owner))
    owner.foreach(cse.owner = _)
    TreeCopiers.copyCase(cse)(guards = guards, body= body)
  }
}


@component(tree, owner)
trait SwitchSymbolAssigner extends SymbolAssignerComponent {
  (switch: SwitchApi)     => {
    val expr  = assign((switch.expr, owner)).asInstanceOf[Expr]
    val cases = switch.cases.map { guard =>
      assign((guard, owner)).asInstanceOf[CaseApi]
    }
    owner.foreach(switch.owner = _)
    TreeCopiers.copySwitch(switch)(cases = cases, expr = expr)
  }
}


@component(tree, owner)
trait LabelSymbolAssigner extends SymbolAssignerComponent {
  (label: LabelApi)     => {
    val stmt  = assign((label.stmt, owner)).asInstanceOf[Expr]
    owner.foreach(label.owner = _)
    TreeCopiers.copyLabel(label)(stmt = stmt)
  }
}

@component(tree, owner)
trait BreakSymbolAssigner extends SymbolAssignerComponent {
  (break: BreakApi)     => {
    owner.foreach(break.owner = _)
    break
  }
}

@component(tree, owner)
trait ContinueSymbolAssigner extends SymbolAssignerComponent {
  (continue: ContinueApi)     => {
    owner.foreach(continue.owner = _)
    continue
  }
}
