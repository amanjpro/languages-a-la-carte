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
Swtich
Label
Break
Continue
*/


trait CaseSymbolAssigner extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case cse: Case     =>
        val guards = cse.guards.map { guard =>
          assign((guard, owner)).asInstanceOf[Expr]
        }
        val body   = assign((cse.body, owner))
        cse.copy(guards = guards, body = body, owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Case, _)    => true
    case _               => false
  }
}

trait SwitchSymbolAssigner extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case switch: Switch     =>
        val expr  = assign((switch.expr, owner)).asInstanceOf[Expr]
        val cases = switch.cases.map { guard =>
          assign((guard, owner)).asInstanceOf[CaseApi]
        }
        switch.copy(cases = cases, expr = expr, owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Switch, _)  => true
    case _               => false
  }
}


trait LabelSymbolAssigner extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case label: Label     =>
        val stmt  = assign((label.stmt, owner)).asInstanceOf[Expr]
        label.copy(stmt = stmt, owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Label, _)   => true
    case _               => false
  }
}

trait BreakSymbolAssigner extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case break: Break     =>
        break.copy(owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Break, _)   => true
    case _               => false
  }
}

trait ContinueSymbolAssigner extends SymbolAssignerComponent {
  def apply(p: (Tree, Option[Symbol])): Tree = {
    val (tree, owner) = p
    tree match {
      case continue: Continue     =>
        continue.copy(owner = owner)
    }
  }

  def isDefinedAt(p: (Tree, Option[Symbol])): Boolean = p match {
    case (_: Continue, _)    => true
    case _                   => false
  }
}
