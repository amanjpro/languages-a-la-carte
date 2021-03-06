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

package ch.usi.inf.l3.sana.arrooj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.ooj
import sana.arrayj
import tiny.ast.{Tree, UseTree}
import ooj.ast.{TreeUtils => OTreeUtils}
import arrayj.ast.{TreeUtils => ATreeUtils}
import TreeExtractors._
import arrayj.ast._

trait TreeUtils extends OTreeUtils {

  override def isTypeUse(tree: UseTree): Boolean = tree match {
    case ArrayTypeUse(tpt)             =>
      isTypeUse(tpt)
    case _                             =>
      super.isTypeUse(tree)
  }

  override def isValidExpression(e: Tree): Boolean = e match {
    case _: ArrayCreationApi => true
    case _                   => super.isValidExpression(e)
  }

  /**
   * Checks if a tree is an array-initialization tree
   *
   * @param e the tree to be checked
   */
  def isArrayInitialization(e: Tree): Boolean =
    ATreeUtils.isArrayInitialization(e)

  /**
   * Checks if a tree is an array-type tree
   *
   * @param e the tree to be checked
   */
  def isArrayTypeUse(e: Tree): Boolean =
    ATreeUtils.isArrayTypeUse(e)

  /**
   * Checks if a tree is an array-access tree
   *
   * @param tree the tree to be checked
   */
  def isArrayAccess(tree: Tree): Boolean = tree match {
    case ArrayAccess(_, _)            => true
    case _                            => false
  }

  /**
   * Checks if a tree is either an array-access or variable access tree
   *
   * @param tree the tree to be checked
   */
  def isArrayAccessOrVariableAccess(tree: Tree): Boolean = tree match {
    case ArrayAccess(array, _)        => isArrayAccessOrVariableAccess(array)
    case tree                         => isVariable(tree)
  }
}

object TreeUtils extends TreeUtils
