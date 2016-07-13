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
import sana.brokenj
import tiny.ast.{Tree, Expr, NamedTree}
import tiny.source.Position
import tiny.types.Type
import tiny.symbols.Symbol
import tiny.names.Name
import tiny.modifiers.Flags
import primj.types.VoidType


/********************* AST Nodes *********************************/

/** A tree to represent a label statement */
trait LabelApi extends Expr with NamedTree {
  /** The name of the label */
  def name: Name
  /**
   * In the statement of the label. The following label:
   * {{{l1: if(b) { ... } else { ... } }}}
   * The `l1` part is the label and the if-else statement is
   * the statement of this label.
   */
  def stmt: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = stmt.bottomUp(z)(f)
    f(r1, this)
  }
}

/** A tree to represent break statements */
trait BreakApi extends Expr {
  /**
   * The label of the break. If the break doesn't have a label,
   * then this label is None.
   */
  def label: Option[Name]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R =
    f(z, this)
}

/** A tree to represent continue statements */
trait ContinueApi extends Expr {
  /**
   * The label of the continue. If the continue doesn't have a label,
   * then this label is None.
   */
  def label: Option[Name]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R =
    f(z, this)
}

/** A tree to represent switch cases */
trait CaseApi extends Tree {
  /**
   * A list of guards of a case. If this case is a default cases, then this
   * list is an empty list.
   */
  def guards: List[Expr]
  /**
   * The body of this case statement
   */
  def body: Tree

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = guards.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r2 = body.bottomUp(r1)(f)
    f(r2, this)
  }
}

// trait DefaultCaseApi extends CaseApi {
//   def pos: Option[Position] = None
// }

/** A tree to represent a switch statement */
trait SwitchApi extends Expr {
  /** The expression of this switch statement */
  def expr: Expr
  /** The cases of this switch statement */
  def cases: List[CaseApi]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = expr.bottomUp(z)(f)
    val r2 = cases.foldLeft(r1)((z, y) => {
      y.bottomUp(z)(f)
    })
    f(r2, this)
  }
}


protected[ast] class Label(val name: Name, val stmt: Expr) extends LabelApi {
  override def toString: String =
    s"Label($name, $stmt)"
}

protected[ast] class Break(val label: Option[Name]) extends BreakApi {
  override def toString: String =
    s"Break($label)"
}

protected[ast] class Continue(val label: Option[Name]) extends ContinueApi {
  override def toString: String =
    s"Continue($label)"
}

protected[ast] class Case(val guards: List[Expr],
  val body: Tree) extends CaseApi {
  override def toString: String =
    s"Case($guards, $body)"
}

protected[ast] class Switch(val expr: Expr,
  val cases: List[CaseApi]) extends SwitchApi {
  override def toString: String =
    s"Switch($expr, $cases)"
}
