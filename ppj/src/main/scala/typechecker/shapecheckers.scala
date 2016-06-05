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

package ch.usi.inf.l3.sana.ppj.typechecker

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj
import sana.arrayj
import sana.ppj
import sana.robustj

import tiny.dsl._
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
