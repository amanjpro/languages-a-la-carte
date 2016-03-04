package ch.usi.inf.l3.sana.arrooj.typechecker

import ch.usi.inf.l3.sana
import sana.arrooj
import sana.ooj
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj


import sana.dsl._
import tiny.ast.{Tree, UseTree}
import tiny.errors.ErrorReporting.{error,warning}
import primj.typechecker.ShapeCheckerComponent
import arrayj.ast.{TreeUtils => _, _}
import arrooj.ast.Implicits._
import arrooj.errors.ErrorCodes._
import arrooj.ast._


// from arrayj

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
trait CastShapeCheckerComponent
  extends primj.typechecker.CastShapeCheckerComponent {
  override protected def isTypeUse(t: UseTree): Boolean =
    TreeUtils.isTypeUse(t)
}

@component
trait MethodDefShapeCheckerComponent
  extends primj.typechecker.MethodDefShapeCheckerComponent {
  override protected def isTypeUse(t: UseTree): Boolean =
    TreeUtils.isTypeUse(t)
}

@component
trait ValDefShapeCheckerComponent
  extends primj.typechecker.ValDefShapeCheckerComponent {
  override protected def isTypeUse(t: UseTree): Boolean =
    TreeUtils.isTypeUse(t)

}


@component
trait ArrayjCreationShapeCheckerComponent
  extends ShapeCheckerComponent {

  (creation: ArrayCreationApi) => {
    creation.size.foreach { size =>
      if(!isValidExpression(size))
        error(BAD_EXPRESSION, "", "", size.pos)
    }
  }

  def isValidExpression(e: Tree): Boolean =
    TreeUtils.isValidExpression(e)
}

@component
trait ArrayInitializerShapeCheckerComponent
  extends ShapeCheckerComponent {

  (init: ArrayInitializerApi) => {
    init.elements.foreach { element =>
      if(!isValidExpressionOrArrayInitializer(element))
        error(BAD_EXPRESSION, "", "", element.pos)
    }
  }

  def isValidExpressionOrArrayInitializer(t: Tree): Boolean =
    TreeUtils.isValidExpression(t) ||
      TreeUtils.isArrayInitialization(t)
}

@component
trait ArrayjAccessShapeCheckerComponent
  extends ShapeCheckerComponent {

  (access: ArrayAccessApi) => {
    if(!isValidExpression(access.array))
      error(BAD_EXPRESSION, "", "", access.array.pos)

    if(!isValidExpression(access.index))
      error(BAD_EXPRESSION, "", "", access.index.pos)
  }

  def isValidExpression(e: Tree): Boolean =
    TreeUtils.isValidExpression(e)
}

@component
trait ArrayjTypeUseShapeCheckerComponent
  extends ShapeCheckerComponent {

  (tuse: ArrayTypeUseApi) => {
    if(!isTypeUse(tuse.tpt))
      error(TYPE_NAME_EXPECTED, "", "", tuse.tpt.pos)
  }

  def isTypeUse(e: UseTree): Boolean =
    TreeUtils.isTypeUse(e)
}


// form ooj

@component
trait ClassDefShapeCheckerComponent extends
  ooj.typechecker.ClassDefShapeCheckerComponent {
  override protected def isTypeUse(tree: UseTree): Boolean =
    TreeUtils.isTypeUse(tree)
}
