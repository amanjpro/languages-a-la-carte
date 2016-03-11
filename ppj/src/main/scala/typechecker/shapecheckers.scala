package ch.usi.inf.l3.sana.ppj.typechecker

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj
import sana.arrayj
import sana.ppj
import sana.robustj

import sana.dsl._
import tiny.ast.{Tree, Expr}
import tiny.errors.ErrorReporting.{error,warning}
import tiny.errors.ErrorCodes._
import tiny.ast.Implicits._
import primj.ast.BlockApi
import primj.typechecker.ShapeCheckerComponent
import ppj.ast._


@component
trait SynchronizedShapeCheckerComponent extends ShapeCheckerComponent {
  (sync: SynchronizedApi)     => {
    check(sync.expr)
    check(sync.block)
    if(!isValidExpression(sync.expr))
      error(BAD_EXPRESSION, "", "", sync.expr.pos)
  }



  protected def isValidExpression(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)
}

@component
trait ValDefShapeCheckerComponent
  extends robustj.typechecker.ValDefShapeCheckerComponent {

  override protected def isSimpleExpression(e: Tree): Boolean =
    TreeUtils.isSimpleExpression(e)
}


@component
trait BlockShapeCheckerComponent
  extends robustj.typechecker.BlockShapeCheckerComponent {

  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)
}

@component
trait IfShapeCheckerComponent
  extends robustj.typechecker.IfShapeCheckerComponent {

  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)
}

@component
trait WhileShapeCheckerComponent
  extends robustj.typechecker.WhileShapeCheckerComponent {

  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)
}


@component
trait ForShapeCheckerComponent
  extends robustj.typechecker.ForShapeCheckerComponent {

  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)
}



@component
trait LabelShapeCheckerComponent
  extends robustj.typechecker.LabelShapeCheckerComponent {

  override protected def canHaveLabel(stmt: Expr): Boolean =
    TreeUtils.canHaveLabel(stmt)
}
