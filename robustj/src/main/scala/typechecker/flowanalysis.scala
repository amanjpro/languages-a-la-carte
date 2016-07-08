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
import sana.arrayj
import sana.ooj
import sana.brokenj
import sana.primj
import sana.calcj
import sana.tiny


import tiny.dsl._
import tiny.core._
import ooj.typechecker.{N, B, CompletenessStatus,
                        FlowCorrectnessCheckerComponent}
import robustj.ast._




/*
Try:
Throw: DONE
Catch: DONE
*/

@component(tree, env)
trait ThrowFlowCorrectnessCheckerComponent
  extends FlowCorrectnessCheckerComponent {
  (thrw: ThrowApi) => {
    check((thrw.expr, env))
    B
  }
}


@component(tree, env)
trait CatchFlowCorrectnessCheckerComponent
  extends FlowCorrectnessCheckerComponent {
  (ctch: CatchApi) => {
    check((ctch.catchClause, env))
  }
}


@component(tree, env)
trait TryFlowCorrectnessCheckerComponent
  extends FlowCorrectnessCheckerComponent {
  (tri: TryApi) => {
    val cenvs  = tri.catches.map(_ => env.duplicate)
    val tr     =check((tri.tryClause, env))
    val z: CompletenessStatus = N
    val cr     = tri.catches.zip(cenvs) match {
      case (c::cs)        =>
        val z = check((c._1, c._2))
        cs.foldLeft(z)((z, y) => z.unify(check((y._1, y._2))))
      case _              =>
        N
    }
    if(cr == N) {
      cenvs.foreach(env.unify(_))
    }
    val res = tri.finallyClause.map(p => check((p, env)))
    res match {
      case Some(fr)            =>
        tr.unify(cr.unify(fr))
      case _                   =>
        tr.unify(cr)
    }
  }
}
