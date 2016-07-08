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
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj
import sana.arrayj
import sana.arrooj
import sana.ooj
import sana.robustj

import tiny.core.TransformationComponent
import tiny.dsl._
import robustj.ast.{TreeFactories, MethodDefApi, TreeCopiers, TreeUpgraders}
import primj.ast.{MethodDefApi => PMethodDefApi}
import tiny.ast.UseTree


@component
trait MethodDefDefTyperComponent extends
    ooj.typechecker.MethodDefDefTyperComponent {
  (mthd: PMethodDefApi) => {
    mthd match {
      case mthd: MethodDefApi                =>
        val res1 = super.apply(mthd).asInstanceOf[ooj.ast.MethodDefApi]
        val throwsClause = mthd.throwsClause.map { tc =>
          typed(tc).asInstanceOf[UseTree]
        }
        val res2 = TreeUpgraders.upgradeMethodDef(res1)
        TreeCopiers.copyMethodDef(res2)(throwsClause = throwsClause)
      case mthd: PMethodDefApi               =>
        val res = TreeUpgraders.upgradeMethodDef(mthd)
        typed(res)
    }
  }
}
