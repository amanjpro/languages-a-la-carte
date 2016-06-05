/*
 * Copyright (c) <2015-2016>, see CONTRIBUTERS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package ch.usi.inf.l3.sana.arrayj.typechecker


import ch.usi.inf.l3.sana
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj


import tiny.dsl._
import tiny.ast.{Tree, UseTree, Expr}
import tiny.errors.ErrorReporting.{error,warning}
import primj.typechecker.ShapeCheckerComponent
import arrayj.ast._
import arrayj.ast.Implicits._
import arrayj.errors.ErrorCodes._

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
trait ArrayCreationShapeCheckerComponent
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
trait ArrayAccessShapeCheckerComponent
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
trait ArrayTypeUseShapeCheckerComponent
  extends ShapeCheckerComponent {

  (tuse: ArrayTypeUseApi) => {
    if(!isTypeUse(tuse.tpt))
      error(TYPE_NAME_EXPECTED, "", "", tuse.tpt.pos)
  }

  def isTypeUse(e: UseTree): Boolean =
    TreeUtils.isTypeUse(e)
}

@component
trait LabelShapeCheckerComponent extends
  brokenj.typechecker.LabelShapeCheckerComponent {


  override protected def canHaveLabel(stmt: Expr): Boolean =
    TreeUtils.canHaveLabel(stmt)
}
