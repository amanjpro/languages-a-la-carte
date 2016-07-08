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

package ch.usi.inf.l3.sana.calcj.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.tiny.ast.Implicits._
import sana.tiny.ast._
import sana.calcj.ast._
import operators._



trait TreeCopiers extends sana.tiny.ast.TreeCopiers {

  /**
   * Returns a copy of a cast tree
   *
   * @param template the cast to be copied
   * @param tpt the type-tree of this tree
   * @param expr the expression of this tree
   */
  def copyCast(template: CastApi)(
      tpt: UseTree = template.tpt,
      expr: Expr = template.expr): CastApi = {
    val res = TreeFactories.mkCast(tpt, expr)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a literal tree
   *
   * @param template the tree to be copied
   * @param constant the constant value of this tree
   */
  def copyLiteral(template: LiteralApi)
      (constant: Constant = template.constant): LiteralApi = {
    val res = TreeFactories.mkLiteral(constant)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a binary tree
   *
   * @param template the tree to be copied
   * @param lhs the left-hand-side expression of this tree
   * @param op the operation of this tree
   * @param rhs the right-hand-side expression of this tree
   */
  def copyBinary(template: BinaryApi)(lhs: Expr = template.lhs,
      op: BOp = template.op, rhs: Expr = template.rhs): BinaryApi = {
    val res = TreeFactories.mkBinary(lhs, op, rhs)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a unary tree
   *
   * @param template the tree to be copied
   * @param isPostfix a flag to indicate if this is a postfix or prefix unary
   *                  operation.
   * @param op the operation of this tree
   * @param expr the expression of this tree
   */
  def copyUnary(template: UnaryApi)(isPostfix: Boolean = template.isPostfix,
    op: UOp = template.op, expr: Expr = template.expr): UnaryApi = {
    val res = TreeFactories.mkUnary(isPostfix, op, expr)
    copyProperties(template, res)
    res
  }

}

object TreeCopiers extends TreeCopiers
