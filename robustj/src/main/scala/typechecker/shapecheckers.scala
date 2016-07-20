/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
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


import tiny.dsl._
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
    if(tri.catches.isEmpty && tri.finallyClause == None) {
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


  /** @see [[robustj.ast.TreeUtils.isValidExpression]] */
  def isValidExpression(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)
}


@component
trait ValDefShapeCheckerComponent
  extends arrooj.typechecker.ValDefShapeCheckerComponent {

  /** @see [[robustj.ast.TreeUtils.isSimpleExpression]] */
  override protected def isSimpleExpression(e: Tree): Boolean =
    TreeUtils.isSimpleExpression(e)

  /** @see [[arrooj.typechecker.ValDefShapeCheckerComponent.sensibleParamFlag]] */
  override protected def sensibleParamFlag(mods: Flags,
    sym: Option[Symbol]): Boolean = sym match {
    case Some(s: ScopeSymbol)   if s.mods.isCatchSymbol  =>
      mods.isExceptionParam && mods.isParam
    case Some(_: MethodSymbol)                           =>
      mods.isParam
    case _                                               =>
      !(mods.isParam || mods.isExceptionParam)
  }
}




@component
trait BlockShapeCheckerComponent
  extends primj.typechecker.BlockShapeCheckerComponent {

  /** @see [[robustj.ast.TreeUtils.isValidStatement]] */
  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)
}

@component
trait IfShapeCheckerComponent
  extends primj.typechecker.IfShapeCheckerComponent {

  /** @see [[robustj.ast.TreeUtils.isValidStatement]] */
  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  /** @see [[robustj.ast.TreeUtils.isValidExpression]] */
  override protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)
}

@component
trait WhileShapeCheckerComponent
  extends primj.typechecker.WhileShapeCheckerComponent {

  /** @see [[robustj.ast.TreeUtils.isValidStatement]] */
  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  /** @see [[robustj.ast.TreeUtils.isValidExpression]] */
  override protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)
}


@component
trait ForShapeCheckerComponent
  extends primj.typechecker.ForShapeCheckerComponent {

  /** @see [[robustj.ast.TreeUtils.isValDefOrStatementExpression]] */
  override protected def isValDefOrStatementExpression(t: Tree): Boolean =
    TreeUtils.isValDefOrStatementExpression(t)

  /** @see [[robustj.ast.TreeUtils.isValidStatement]] */
  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  /** @see [[robustj.ast.TreeUtils.isValidExpression]] */
  override protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)
}

@component
trait ArrayCreationShapeCheckerComponent
  extends arrooj.typechecker.ArrayCreationShapeCheckerComponent {
  /** @see [[robustj.ast.TreeUtils.isValidExpression]] */
  override def isValidExpression(e: Tree): Boolean =
    TreeUtils.isValidExpression(e)
}

@component
trait ArrayAccessShapeCheckerComponent
  extends arrooj.typechecker.ArrayAccessShapeCheckerComponent {

  /** @see [[robustj.ast.TreeUtils.isValidExpression]] */
  override def isValidExpression(e: Tree): Boolean =
    TreeUtils.isValidExpression(e)
}

@component
trait LabelShapeCheckerComponent extends
  brokenj.typechecker.LabelShapeCheckerComponent {

  /** @see [[robustj.ast.TreeUtils.canHaveLabel]] */
  override protected def canHaveLabel(stmt: Expr): Boolean =
    TreeUtils.canHaveLabel(stmt)
}
