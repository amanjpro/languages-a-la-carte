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

package ch.usi.inf.l3.sana.robustj.eval

import ch.usi.inf.l3.sana
import sana.robustj
import sana.arrooj
import sana.arrayj
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.TransformationComponent
import tiny.dsl._
import ooj.eval.ConstantFoldingComponent
import robustj.ast._
import tiny.ast.Expr
import primj.ast.{BlockApi, MethodDefApi => PMethodDefApi}





@component(tree, env)
trait MethodDefConstantFoldingComponent
  extends ConstantFoldingComponent {
  (mthd: PMethodDefApi) => {
    mthd match {
      case mthd: MethodDefApi                 =>
        val (body, newEnv) = constantFold((mthd.body, env))
        (TreeCopiers.copyMethodDef(mthd)(body = body.asInstanceOf[Expr]),
          newEnv)
      case mthd: PMethodDefApi               =>
        val res = TreeUpgraders.upgradeMethodDef(mthd)
        constantFold((res, env))
    }
  }
}




@component(tree, env)
trait TryConstantFoldingComponent extends ConstantFoldingComponent {
  (tri: TryApi) => {
    val (tryClause, env1) = constantFold((tri.tryClause, env))
    val (catches, env2)   = tri.catches.foldLeft((Nil: List[CatchApi], env1)){
      (z, init) =>
        val inits   = z._1
        val e       = z._2
        val (res, env) = constantFold((init, e))
        (inits++List(res.asInstanceOf[CatchApi]), env)
    }
    tri.finallyClause match {
      case Some(finallyClause)      =>
        val (fc, env3) = constantFold((finallyClause, env2))
        ((TreeCopiers.copyTry(tri)(tryClause = tryClause.asInstanceOf[BlockApi],
          catches, Some(fc.asInstanceOf[BlockApi])), env3))
      case _                        =>
        ((TreeCopiers.copyTry(tri)(tryClause = tryClause.asInstanceOf[BlockApi],
          catches, None), env2))
    }
  }
}

@component(tree, env)
trait ThrowConstantFoldingComponent
  extends ConstantFoldingComponent {
  (thrw: ThrowApi) => {
    val (newExpr, newEnv) = constantFold((thrw.expr, env))
    val newThrow = TreeCopiers.copyThrow(thrw)(expr = newExpr.asInstanceOf[Expr])
    (newThrow, newEnv)
  }
}


@component(tree, env)
trait CatchConstantFoldingComponent
  extends ConstantFoldingComponent {
  (ctch: CatchApi) => {
    val (catchClause, newEnv) = constantFold((ctch.catchClause, env))
    (TreeCopiers.copyCatch(ctch)(
      catchClause = catchClause.asInstanceOf[BlockApi]), newEnv)
  }
}
