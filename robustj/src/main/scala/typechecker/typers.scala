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
import calcj.typechecker.TyperComponent
import robustj.ast._
import tiny.ast.{Tree, NoTree, Expr, UseTree}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast.{ValDefApi, BlockApi}
import tiny.types.Type
import robustj.ast._
import primj.ast.{MethodDefApi => PMethodDefApi}
import robustj.errors.ErrorCodes._
import robustj.types.TypeUtils
import robustj.modifiers.Ops._
import arrooj.ast.Implicits._




@component
trait ThrowTyperComponent extends TyperComponent {
  (thrw: ThrowApi) => {
    val expr = typed(thrw.expr).asInstanceOf[Expr]
    checkThrownExpressionType(expr)
    TreeCopiers.copyThrow(thrw)(expr = expr)
  }
  protected def checkThrownExpressionType(expr: Expr): Unit = {
    expr.tpe.foreach { tpe =>
      if(!(tpe <:< throwableClassType))
        error(THROWING_NON_THROWABLE, "", "", expr.pos)
    }
  }

  protected def throwableClassType: Type =
    TypeUtils.throwableClassType
}


@component
trait TryTyperComponent extends TyperComponent {
  (tri: TryApi) => {
    val tryClause     = typed(tri.tryClause).asInstanceOf[BlockApi]
    val catches       =
      tri.catches.map(c => typed(c).asInstanceOf[CatchApi])
    val finallyClause =
      tri.finallyClause.map(fc => typed(fc).asInstanceOf[BlockApi])

    TreeCopiers.copyTry(tri)(tryClause = tryClause,
      catches = catches, finallyClause = finallyClause)
  }
}


@component
trait CatchTyperComponent extends TyperComponent {
  (ctch: CatchApi) => {
    val eparam        = typed(ctch.eparam).asInstanceOf[ValDefApi]
    checkCatchParamType(eparam)
    val catchClause   = typed(ctch.catchClause).asInstanceOf[BlockApi]

    TreeCopiers.copyCatch(ctch)(eparam = eparam,
      catchClause = catchClause)
  }

  protected def checkCatchParamType(param: ValDefApi): Unit = {
    param.tpe.foreach { tpe =>
      if(!(tpe <:< throwableClassType))
        error(CATCHING_NON_THROWABLE, "", "", param.pos)
    }
  }
  protected def throwableClassType: Type =
    TypeUtils.throwableClassType
}


@component
trait MethodDefTyperComponent
  extends ooj.typechecker.MethodDefTyperComponent {

  (mthd: PMethodDefApi) => {
    mthd match {
      case mthd: MethodDefApi                =>
        val throwsClause =
          mthd.throwsClause.map(id => typed(id).asInstanceOf[UseTree])
        val res1         =
          super.apply(mthd).asInstanceOf[PMethodDefApi]
        val res2 = TreeUpgraders.upgradeMethodDef(res1)
        TreeCopiers.copyMethodDef(res2)(throwsClause = throwsClause)
      case mthd: PMethodDefApi               =>
        val res = TreeUpgraders.upgradeMethodDef(mthd)
        typed(res)
    }
  }

  override def allPathsReturn(expr: Tree): Boolean = {
    enclosingMethod(expr.symbol) match {
      case Some(mthd)                         =>
        mthd.mods.isAbstract || TreeUtils.allPathsReturn(expr)
      case None                               =>
        expr == NoTree
    }
  }

  protected def checkThrowsClause(throwsClause: List[UseTree]): Unit = for {
    id  <- throwsClause
    tpe <- id.tpe
  } {
    if(!(tpe <:< throwableClassType))
      error(NON_THROWABLE_IN_THROWS_CLAUSE, tpe.toString,
        throwableClassType.toString, id.pos)
  }

  protected def throwableClassType: Type =
    TypeUtils.throwableClassType
}
