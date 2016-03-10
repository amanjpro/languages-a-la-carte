package ch.usi.inf.l3.sana.robustj.typechecker

import ch.usi.inf.l3.sana
import sana.robustj
import sana.arrooj
import sana.ooj
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj


import sana.dsl._
import tiny.ast.{Tree, Expr, UseTree}
import tiny.errors.ErrorReporting.{error,warning}
import primj.typechecker.ShapeCheckerComponent
import primj.symbols.{MethodSymbol,ScopeSymbol}
import tiny.modifiers.Flags
import robustj.modifiers.Ops._
import tiny.symbols.Symbol
import arrooj.ast.Implicits._
import robustj.errors.ErrorCodes._
import robustj.ast._


@component
trait TryShapeCheckerComponent extends ShapeCheckerComponent {
  (tri: TryApi)     => {
    check(tri.tryClause)
    tri.catches.foreach(check(_))
    tri.finallyClause.foreach(check(_))
    if(tri.catches.isEmpty) {
      error(NO_CATCH_FOUND, "", "", tri.pos)
    }
  }
}


@component
trait CatchShapeCheckerComponent extends ShapeCheckerComponent {
  (ctch: CatchApi)      => {
    check(ctch.eparam)
    check(ctch.catchClause)
  }
}


@component
trait ThrowShapeCheckerComponent extends ShapeCheckerComponent {
  (thrw: ThrowApi)    => {
    if(!isValidExpression(thrw.expr))
      error(BAD_EXPRESSION, "", "", thrw.expr.pos)
  }


  def isValidExpression(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)
}


@component
trait ValDefShapeCheckerComponent
  extends arrooj.typechecker.ValDefShapeCheckerComponent {

  override protected def isSimpleExpression(e: Tree): Boolean =
    TreeUtils.isSimpleExpression(e)

  override protected def sensibleParamFlag(mods: Flags,
    sym: Option[Symbol]): Boolean = sym match {
    case Some(s: ScopeSymbol)   if s.mods.isCatchSymbol  =>
      mods.isExceptionParam
    case Some(_: MethodSymbol)                           =>
      mods.isParam
    case _                                               =>
      !(mods.isParam || mods.isExceptionParam)
  }
}




@component
trait BlockShapeCheckerComponent
  extends primj.typechecker.BlockShapeCheckerComponent {

  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)
}

@component
trait IfShapeCheckerComponent
  extends primj.typechecker.IfShapeCheckerComponent {

  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  override protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)
}

@component
trait WhileShapeCheckerComponent
  extends primj.typechecker.WhileShapeCheckerComponent {

  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  override protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)
}


@component
trait ForShapeCheckerComponent
  extends primj.typechecker.ForShapeCheckerComponent {

  override protected def isValDefOrStatementExpression(t: Tree): Boolean =
    TreeUtils.isValDefOrStatementExpression(t)

  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  override protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)
}

@component
trait ArrayCreationShapeCheckerComponent
  extends arrooj.typechecker.ArrayCreationShapeCheckerComponent {
  override def isValidExpression(e: Tree): Boolean =
    TreeUtils.isValidExpression(e)
}

@component
trait ArrayAccessShapeCheckerComponent
  extends arrooj.typechecker.ArrayAccessShapeCheckerComponent {

    override def isValidExpression(e: Tree): Boolean =
    TreeUtils.isValidExpression(e)
}

@component
trait LabelShapeCheckerComponent extends
  brokenj.typechecker.LabelShapeCheckerComponent {

  override protected def canHaveLabel(stmt: Expr): Boolean =
    TreeUtils.canHaveLabel(stmt)
}
