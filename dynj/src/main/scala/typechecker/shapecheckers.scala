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

package ch.usi.inf.l3.sana.dynj.typechecker


import ch.usi.inf.l3.sana
import sana.dynj
import sana.robustj
import sana.arrooj
import sana.ooj
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj


import tiny.dsl._
import tiny.ast.Tree
import calcj.ast.BinaryApi
import tiny.errors.ErrorReporting.{error,warning}
import primj.typechecker.ShapeCheckerComponent
import tiny.ast.Implicits._
import dynj.errors.ErrorCodes._
import dynj.ast.operators.InstanceOf
import robustj.ast.TreeUtils

@component
trait BinaryShapeCheckerComponent extends ShapeCheckerComponent {
  (bin: BinaryApi)     => {
    check(bin.lhs)
    check(bin.rhs)
    if(!isValidExpression(bin.lhs))
      error(BAD_EXPRESSION, "", "", bin.lhs.pos)

    bin.op match {
      case InstanceOf   if !isTypeUse(bin.rhs)         =>
        error(BAD_EXPRESSION, "", "", bin.rhs.pos)
      case _            if !isValidExpression(bin.rhs) =>
        error(BAD_EXPRESSION, "", "", bin.rhs.pos)
      case _                                           =>
        ()
    }
  }


  /**
   * Checks if a tree is a type-use
   *
   * @param t the tree to be checked
   */
  protected def isTypeUse(t: Tree): Boolean = t match {
    case use: tiny.ast.UseTree => TreeUtils.isTypeUse(use)
    case _                     => false
  }

  /** @see {{{tiny.ast.TreeUtils.isValidExpression}}} */
  protected def isValidExpression(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)
}
