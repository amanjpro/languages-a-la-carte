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

package ch.usi.inf.l3.sana.brokenj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import primj.ast
import tiny.ast.{Tree, NoTree}
import primj.ast._


trait TreeUtils extends ast.TreeUtils {

  override def isSimpleExpression(tree: Tree): Boolean = tree match {
    case _: ContinueApi                                   => false
    case _: BreakApi                                      => false
    case _: CaseApi                                       => false
    case _: SwitchApi                                     => false
    case _: LabelApi                                      => false
    case _                                             =>
      super.isSimpleExpression(tree)
  }

  override def allPathsReturn(tree: Tree): Boolean =
    allPathsReturnAux(tree, allPathsReturn)

  override protected def allPathsReturnAux(tree: Tree,
        recurse: Tree => Boolean): Boolean = tree match {
    // brokenj
    case _: ContinueApi | _: BreakApi                        =>
      false
    case label: LabelApi                                  =>
      recurse(label.stmt)
    case cse: CaseApi                                     =>
      allPathsReturn(cse.body)
    case switch: SwitchApi                                =>
      switch.cases.foldLeft(true)((z, y) =>
        z || recurse(y)
      )
    case e                                              =>
      super.allPathsReturnAux(e, recurse)
  }

  def canHaveLabel(tree: Tree): Boolean = tree match {
    // INFO: Synchronize, Throw and Try to be added
    case _: LabelApi | _: IfApi | _: WhileApi | _: ForApi | _: BlockApi |
            NoTree | _: SwitchApi | _: ContinueApi | _: BreakApi  |
         _: ReturnApi                                         =>
      true
    case e                                                 =>
      isValidStatementExpression(e)
  }

  def isContinuable(tree: Tree): Boolean = isLoopTree(tree)

  def isBreakable(tree: Tree): Boolean = tree match {
    case _: CaseApi | _: WhileApi | _: ForApi      => true
    case _                                         => false
  }

  override def isValidStatement(e: Tree): Boolean = e match {
    case _: SwitchApi | _: LabelApi | _: ContinueApi | _: BreakApi =>
      true
    case _                                                         =>
      super.isValidStatement(e)
  }

  def isLoopTree(tree: Tree): Boolean = tree match {
    case _: WhileApi | _: ForApi            => true
    case _                                  => false
  }

}

object TreeUtils extends TreeUtils
