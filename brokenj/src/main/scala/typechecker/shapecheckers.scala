package ch.usi.inf.l3.sana.brokenj.typechecker


import ch.usi.inf.l3.sana
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.CheckerComponent
import sana.dsl._
import tiny.ast._
import tiny.ast.Implicits._
import calcj.ast._
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.symbols._
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._
import primj.typechecker.ShapeCheckerComponent
import brokenj.ast._
import brokenj.ast.TreeUtils


@component
trait LabelShapeCheckerComponent extends ShapeCheckerComponent {
  (lbl: Label) => {
    if(canHaveLabel(lbl.stmt)) {
      ()
    } else {
      error(UNEXPETED_TREE,
          lbl.stmt.toString, "an expression", lbl.stmt.pos, lbl.stmt)
    }
    check(lbl.stmt)
  }


  protected def canHaveLabel(stmt: Expr): Boolean =
    TreeUtils.canHaveLabel(stmt)
}



@component
trait SwitchShapeCheckerComponent extends ShapeCheckerComponent {
  (switch: Switch) => {
    check(switch.expr)
    switch.cases.foreach(check(_))
  }
}


@component
trait CaseShapeCheckerComponent extends ShapeCheckerComponent {
  (cse: Case) => {
    check(cse.body)
    cse.guards.foreach(check(_))
  }
}
