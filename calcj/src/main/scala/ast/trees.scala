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
import sana.tiny
import sana.calcj

import tiny.ast._
import tiny.types._
import tiny.source.Position
import tiny.symbols.Symbol

import calcj.types._
import operators._




/**
 * A tree to represent a cast operation like:
 * {{{(tpt) expr}}}
 */
trait CastApi extends Expr {
  def tpt: UseTree
  def expr: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = tpt.bottomUp(z)(f)
    val r2 = expr.bottomUp(r1)(f)
    f(r2, this)
  }
}

/** A tree to represent literal values */
trait LiteralApi extends Expr {
  def constant: Constant

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = f(z, this)
}

/** A tree to represent binary operations */
trait BinaryApi extends Expr {
  def lhs: Expr
  def op: BOp
  def rhs: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = lhs.bottomUp(z)(f)
    val r2 = rhs.bottomUp(r1)(f)
    f(r2, this)
  }
}

/** A tree to represent unary operations */
trait UnaryApi extends Expr {
  def isPostfix: Boolean
  def op: UOp
  def expr: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = expr.bottomUp(z)(f)
    f(r1, this)
  }
}



protected[ast] class Cast(val tpt: UseTree,
  val expr: Expr) extends CastApi {
  override def toString: String =
    s"Cast(${tpt.toString}, ${expr.toString})"
}


protected[ast] class Literal(val constant: Constant) extends LiteralApi {
  override def toString: String =
    s"Literal(${constant.toString})"
}


protected[ast] class Binary(val lhs: Expr,
  val op: BOp, val rhs: Expr) extends BinaryApi {
  override def toString: String =
    s"Binary(${lhs.toString}, ${op.toString}, ${rhs.toString})"
}

protected[ast] class Unary(val isPostfix: Boolean,
  val op: UOp, val expr: Expr) extends UnaryApi {
  override def toString: String =
    s"Unary(${isPostfix.toString}, ${op.toString}, ${expr.toString})"
}
