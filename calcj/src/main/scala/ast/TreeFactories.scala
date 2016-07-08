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



trait TreeFactories extends sana.tiny.ast.TreeFactories {

  /**
   * Creates a cast tree.
   *
   * @param tpt the type-tree of this cast
   * @param expr the expression of this cast
   * @param pos the position of this cast
   */
  def mkCast(tpt: UseTree, expr: Expr,
           pos: Option[Position] = None): CastApi = {
    val res = new Cast(tpt, expr)
    tpt.tpe.foreach(res.tpe = _)
    expr.owner.foreach(res.owner = _)
    pos.foreach(res.pos = _)
    res
  }

  /**
   * Creates a literal tree.
   *
   * @param constant the constant value of this tree
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def mkLiteral(constant: Constant,
              pos: Option[Position] = None,
              owner: Option[Symbol] = None): LiteralApi = {
    val res = new Literal(constant)
    // res.tpe = constant.tpe
    owner.foreach(res.owner = _)
    pos.foreach(res.pos = _)
    res
  }

  /**
   * Creates a binary tree.
   *
   * @param lhs the left-hand-side expression of this tree
   * @param op the operation of this tree
   * @param rhs the right-hand-side expression of this tree
   * @param pos the position of this tree
   * @param tpe the tpe of this tree
   * @param owner the owner of this tree
   */
  def mkBinary(lhs: Expr, op: BOp, rhs: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): BinaryApi = {
    val res = new Binary(lhs, op, rhs)
    owner.foreach(res.owner = _)
    tpe.foreach(res.tpe = _)
    pos.foreach(res.pos = _)
    res
  }

  /**
   * Creates a unary tree.
   *
   * @param isPostfix a flag to indicate weather this is a postfix or prefix unary
   *                  operation
   * @param op the operation of this tree
   * @param rhs the expression of this tree
   * @param pos the position of this tree
   * @param tpe the tpe of this tree
   * @param owner the owner of this tree
   */
  def mkUnary(isPostfix: Boolean, op: UOp, expr: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): UnaryApi = {
    val res = new Unary(isPostfix, op, expr)
    owner.foreach(res.owner = _)
    tpe.foreach(res.tpe = _)
    pos.foreach(res.pos = _)
    res
  }

}

object TreeFactories extends TreeFactories
