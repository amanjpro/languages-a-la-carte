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

package ch.usi.inf.l3.sana.ooj.eval


import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.ast.Expr
import sana.calcj.ast.LiteralApi

/** A representation of compile time values */
trait Value
/** An expression value (non-constant expression) */
case class ExprValue(value: Expr) extends Value
/** A literal (constant) value (constant expression) */
case class LiteralValue(value: LiteralApi) extends Value

/** This environment is used for constant folding */
trait Env {
  /** A map between symbols to their values */
  protected val bindings: Map[Symbol, Value]

  /**
   * Binds a symbol to a value
   *
   * @param symbol the symbol to bound
   * @param value the value of the symbol
   */
  def bind(symbol: Symbol, value: Value): Env = {
    val newBindings = bindings + (symbol -> value)
    create(newBindings)
  }


  /**
   * Checks if a symbol is already bound in the current environment
   *
   * @param symbol the symbol to be checked
   */
  protected def defines(symbol: Symbol): Boolean =
    bindings.contains(symbol)

  /**
   * Returns the bound value of a symbol
   *
   * @param symbol the symbol to return its bound value
   */
  def getValue(symbol: Symbol): Option[Value] =
    bindings.get(symbol)


  /**
   * Unbinds a symbol to a value
   *
   * @param symbol the symbol to unbound
   */
  def unbind(symbol: Symbol): Env = {
    val newBindings = bindings - symbol
    create(newBindings)
  }

  /**
   * Creates a new instance of environment
   *
   * @param bindings the initial bindings of the new environment
   */
  protected def create(bindings: Map[Symbol, Value]): Env


  override def hashCode: Int = bindings.hashCode
  override def equals(other: Any): Boolean = other match {
    case null      =>
      false
    case that: Env =>
      bindings == that.bindings
    case _         =>
      false
  }
}


object Env {
  private class EnvImpl(
      protected val bindings: Map[Symbol, Value]) extends Env {
    protected def create(bindings: Map[Symbol, Value]): Env =
      new EnvImpl(bindings)
  }

  def apply(bindings: Map[Symbol, Value]): Env =
    new EnvImpl(bindings)

  /** Returns an empty environment */
  def emptyEnv: Env =
    apply(Map.empty)
}
