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

package ch.usi.inf.l3.sana.modulej.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj
import sana.robustj
import sana.modulej

import tiny.modifiers.Ops._
import ooj.ast.{CompilationUnitApi => OCompilationUnitApi}
import modulej.ast.{CompilationUnitApi => MCompilationUnitApi}

trait TreeUpgraders extends robustj.ast.TreeUpgraders {
  /**
   * Upgrades an instance of [[ooj.ast.CompilationUnitApi]] to
   * [[modulej.ast.CompilationUnitApi]] which adds the list of imports to it
   *
   * @param cunit the tree to be upgraded
   */

  def upgradeCompilationUnit(
    cunit: OCompilationUnitApi): MCompilationUnitApi =
    cunit match {
      case cunit: MCompilationUnitApi         => cunit
      case cunit: OCompilationUnitApi         =>
        val res = TreeFactories.mkCompilationUnit(Nil,
          cunit.module, cunit.sourceName, cunit.sourcePath)
        res.attributes = cunit.attributes
        res
    }
}


object TreeUpgraders extends TreeUpgraders
