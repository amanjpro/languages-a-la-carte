package ch.usi.inf.l3.sana.brokenj.namers

import ch.usi.inf.l3.sana
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast._
import tiny.symbols._
import brokenj.ast._
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast._
import primj.ast.TreeUtils
import primj.symbols._
import primj.namers.NamerComponent
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._

/*
Done from primj
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

trait CaseNamerComponent extends NamerComponent {
 def apply(tree: Tree): Tree = tree match {
    case cse: Case            =>
      val guards =
        cse.guards.map(x => name(x).asInstanceOf[Expr])
      val body   = name(cse.body)
      cse.copy(guards = guards, body = body)
  }

  def isDefinedAt(tree: Tree): Boolean   = defines(tree, "Case")
}

trait SwitchNamerComponent extends NamerComponent {
 def apply(tree: Tree): Tree = tree match {
    case switch: Switch            =>
      val cases =
        switch.cases.map(x => name(x).asInstanceOf[CaseApi])
      val expr   = name(switch.expr).asInstanceOf[Expr]
      switch.copy(cases = cases, expr = expr)
  }

  def isDefinedAt(tree: Tree): Boolean   = defines(tree, "Switch")
}

trait LabelNamerComponent extends NamerComponent {
 def apply(tree: Tree): Tree = tree match {
    case label: Label            =>
      val stmt   = name(label.stmt).asInstanceOf[Expr]
      label.copy(stmt = stmt)
  }

  def isDefinedAt(tree: Tree): Boolean   = defines(tree, "Label")
}

trait BreakNamerComponent extends NamerComponent {
 def apply(tree: Tree): Tree = tree match {
    case break: Break            => break
  }

  def isDefinedAt(tree: Tree): Boolean   = defines(tree, "Break")
}

trait ContinueNamerComponent extends NamerComponent {
 def apply(tree: Tree): Tree = tree match {
    case continue: Continue => continue
  }

  def isDefinedAt(tree: Tree): Boolean   = defines(tree, "Continue")
}

