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

package ch.usi.inf.l3.sana.primj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.ast._
import tiny.source.Position
import tiny.types.Type
import tiny.names.Name
import tiny.symbols.Symbol
import tiny.modifiers.Flags
import primj.types._


/** A trait to represent a program */
trait ProgramApi extends Tree {
  /** List of definitions defined in this program. */
  def members: List[DefTree]
  /**
   * The name of the source file of this program file.
   * In primj every program consists of a single file, only */
  def sourceName: String

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = members.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    f(r1, this)
  }
}

// Variable and Method definitions
// At this stage, methods don't have modifiers (no encapsulation,
// no final). But ValDefs have, since they can be either params, normal
// variables or final variables.

/** A trait to represent a method definition */
trait MethodDefApi extends TermTree {
  /** The return type-tree of a method */
  def ret: UseTree

  /** A list of parameters of a method */
  def params: List[ValDefApi]

  /** A body of a method */
  def body: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = ret.bottomUp(z)(f)
    val r2 = params.foldLeft(r1)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r3 = body.bottomUp(r2)(f)
    f(r3, this)
  }
}

/** A trait to represent variable definitions */
trait ValDefApi extends TermTree {
  /** The modifiers of a variable, like: static, public and others */
  def mods: Flags
  /** The type-tree of the variable. {{{int}}} is the type-tree in the following
   * expression: {{{int v = e}}}.
   */
  def tpt: UseTree
  /** The right-hand-side of a variable */
  def rhs: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = tpt.bottomUp(z)(f)
    val r2 = rhs.bottomUp(r1)(f)
    f(r2, this)
  }
}



/** A tree to represent return statements */
trait ReturnApi extends Expr {
  /**
   * The expression of the return, this can be None to support return
   * statements without expressions.
   */
  val expr: Option[Expr]
  /** Is this a void return? Meaning it has no expression */
  def isVoid: Boolean = expr == None

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = expr.map(_.bottomUp(z)(f)).getOrElse(z)
    f(z, this)
  }
}

/** A tree to represent a block of a statements. */
trait BlockApi extends Expr {
  /** The list of statements in this block */
  def stmts: List[Tree]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = stmts.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    f(r1, this)
  }

}



/** A tree to represent assignment expressions, be it compound or not. */
trait AssignApi extends Expr {
  /** the left-hand side expression of this assignment expression */
  def lhs: Expr
  /** the right-hand side expression of this assignment expression */
  def rhs: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = lhs.bottomUp(z)(f)
    val r2 = rhs.bottomUp(r1)(f)
    f(r2, this)
  }
}

/** A tree to represent if-else statements */
trait IfApi extends Expr {
  /** The condition of if-else */
  def cond: Expr
  /** The then clause of if-else */
  def thenp: Expr
  /** The else clause of if-else */
  def elsep: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = cond.bottomUp(z)(f)
    val r2 = thenp.bottomUp(r1)(f)
    val r3 = elsep.bottomUp(r2)(f)
    f(r3, this)
  }

}


/** A tree to represent while and do-while loops */
trait WhileApi extends Expr {
  /** Is this tree a do-while loop? */
  def isDoWhile: Boolean
  /** The condition of the loop */
  def cond: Expr
  /** The body of the loop */
  def body: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = cond.bottomUp(z)(f)
    val r2 = body.bottomUp(r1)(f)
    f(r2, this)
  }
}

/** A tree to represent a for-loop tree */
trait ForApi extends Expr {
  /** The initialization statements of the loop */
  def inits: List[Tree]
  /** The condition expression of the loop */
  def cond: Expr
  /** The steps expressions of the loop */
  def steps: List[Expr]
  /** The body of the loop */
  def body: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = inits.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r2 = cond.bottomUp(r1)(f)
    val r3 = steps.foldLeft(r2)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r4 = body.bottomUp(r2)(f)

    f(r4, this)
  }
}

// Ternary operator
/**
 * A tree to represent ternary expressions like {{{cond? thenp: elsep}}}, the
 * difference ternary expressions and if-else statements is that, ternary
 * expressions have a value and type, while if-else statements do not.
 */
trait TernaryApi extends Expr {
  /** the condition of this expression */
  def cond: Expr
  /** the then-clause of this expression */
  def thenp: Expr
  /** the else-clause of this expression */
  def elsep: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = cond.bottomUp(z)(f)
    val r2 = thenp.bottomUp(r1)(f)
    val r3 = elsep.bottomUp(r2)(f)
    f(r3, this)
  }
}

// Apply
/** A tree to represent function/method application */
trait ApplyApi extends Expr {
  /** The function/method that is applied */
  def fun: Expr
  /** The list of arguments of this method application */
  def args: List[Expr]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = fun.bottomUp(z)(f)
    val r2 = args.foldLeft(r1)((z, y) => {
      y.bottomUp(z)(f)
    })

    f(r2, this)
  }
}

protected[ast] class Program(val members: List[DefTree],
  val sourceName: String) extends ProgramApi {
  override def toString: String =
    s"Program($members, $sourceName)"
}

protected[ast] class Assign(val lhs: Expr, val rhs: Expr) extends AssignApi {
  override def toString: String =
    s"Assign($lhs, $rhs)"
}

protected[ast] class If(val cond: Expr, val thenp: Expr,
  val elsep: Expr) extends IfApi {
  override def toString: String =
    s"If(${cond.toString}, ${thenp.toString}, ${elsep.toString})"
}


protected[ast] class While(val isDoWhile: Boolean, val cond: Expr,
  val body: Expr) extends WhileApi {
  override def toString: String =
    s"While(${cond.toString}, ${body.toString})"
}

protected[ast] class Block(val stmts: List[Tree]) extends BlockApi {
  override def toString: String =
    s"Block(${stmts.toString})"
}

protected[ast] class For(val inits: List[Tree],
  val cond: Expr, val steps: List[Expr], val body: Expr) extends ForApi {
  override def toString: String =
    s"For($inits, $cond, $steps, $body)"
}

protected[ast] class Ternary(val cond: Expr, val thenp: Expr,
  val elsep: Expr) extends TernaryApi {
  override def toString: String =
    s"Ternary($cond, $thenp, $elsep)"
}

protected[ast] class Apply(val fun: Expr,
  val args: List[Expr]) extends ApplyApi {
  override def toString: String =
    s"Apply($fun, $args)"
}

protected[ast] class Return(val expr: Option[Expr]) extends ReturnApi {
  override def toString: String =
    s"Return($expr)"
}

protected[ast] class MethodDef(val ret: UseTree,
  val name: Name, val params: List[ValDefApi],
  val body: Expr) extends MethodDefApi {
  override def toString: String =
    s"MethodDef($ret, $name, $params, $body)"
}

protected[ast] class ValDef(val mods: Flags,
  val tpt: UseTree, val name: Name, val rhs: Expr) extends ValDefApi {
  override def toString: String =
    s"ValDef($mods, $tpt, $name, $rhs)"
}
