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

package ch.usi.inf.l3.sana.robustj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj
import sana.robustj

import tiny.modifiers.Ops._
import primj.ast.{MethodDefApi => PMethodDefApi}
import ooj.ast.{MethodDefApi => OMethodDefApi}
import robustj.ast.{MethodDefApi => RMethodDefApi}

/**
 * A trait to upgrade a tree from an old version to its upgraded version.
 * A tree is upgraded when it is extended to add new features to it, for
 * example {{{MethodDefApi}}} in `ooj` didn't have declared exceptions (aka
 * throws clause), but in `robustj` we added them to it. Which means we
 * upgraded (extended) it to add this feature to it.
 */
trait TreeUpgraders {
  /**
   * Upgrades an instance of [[primj.ast.MethodDefApi]], [[ooj.ast.MethodDefApi]] to
   * [[robustj.ast.MethodDefApi]]
   *
   * @param mthd the tree to be upgraded
   */
  def upgradeMethodDef(mthd: PMethodDefApi): RMethodDefApi = mthd match {
    case mthd: RMethodDefApi             => mthd
    case mthd: OMethodDefApi             =>
      val res         = TreeFactories.mkMethodDef(mthd.mods,
                                                  mthd.ret,
                                                  mthd.name,
                                                  mthd.params,
                                                  Nil,
                                                  mthd.body)
      res.attributes = mthd.attributes
      res
    case _                               =>
      val res         = TreeFactories.mkMethodDef(noflags,
                                                  mthd.ret,
                                                  mthd.name,
                                                  mthd.params,
                                                  Nil,
                                                  mthd.body)
      res.attributes = mthd.attributes
      res
  }
}


object TreeUpgraders extends TreeUpgraders
