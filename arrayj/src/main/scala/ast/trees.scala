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

package ch.usi.inf.l3.sana.arrayj.ast

import ch.usi.inf.l3.sana
import sana.tiny

import tiny.ast.{Tree, Expr, UseTree}
import tiny.names.Name

/********************* AST Nodes *********************************/

/** A tree to represent an array initialization */
trait ArrayInitializerApi extends Expr {
  /** The elements of this array initialization */
  def elements: List[Expr]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = elements.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    f(r1, this)
  }
}


/** A tree to represent array-accesses */
trait ArrayAccessApi extends Expr {
  /** The array to be accessed */
  def array: Expr
  /** The index of which we want to access from [[ArrayAccessApi.array]] */
  def index: Expr


  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = index.bottomUp(z)(f)
    f(r1, this)
  }
}

/**
 * A tree to represent an array type-use.  As an example the return-type tree
 * of the following method is represented as an instance of this tree.
 * {{{
 * int[] m() {
 *   ...
 * }
 * }}}
 */
trait ArrayTypeUseApi extends UseTree {
  /**
   * The type-tree of this array.
   * In the following expression, {{{tpt}}} represents `int`:
   * {{{int[]}}}
   */
  def tpt: UseTree
  /** The name of this tree, same as the name of [[ArrayTypeUseApi.tpt]] */
  def name: Name = tpt.name


  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    f(z, this)
  }
}


/** An array to represent array creation expressions */
trait ArrayCreationApi extends Expr {
  /**
   * The array that this tree creates. In the following example:
   * {{{new int[5]}}}
   * `int` is represented by this field.
   */
  def array: Expr
  /**
   * The size of this array that we create. It is of type Option,
   * because we can have the size missing, as in the second dimension of
   * the following example:
   * {{{new int[5][]}}}
   */
  def size: Option[Expr]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, array)
    val r2 = size.map(f(r1, _)).getOrElse(r1)
    f(r2, this)
  }
}








protected[ast] class ArrayInitializer(val elements: List[Expr]) extends
  ArrayInitializerApi {

  override def toString: String = s"ArrayInitializer($elements)"
}

protected[ast] class ArrayAccess(val array: Expr,
  val index: Expr) extends ArrayAccessApi {

  override def toString: String = s"ArrayAccess($array, $index)"
}

protected[ast] class ArrayTypeUse(val tpt: UseTree) extends ArrayTypeUseApi {
  override def toString: String = s"ArrayTypeUse($tpt)"
}

protected[ast] class ArrayCreation(val array: Expr,
    val size: Option[Expr]) extends ArrayCreationApi {
  override def toString: String = s"ArrayCreation($array, $size)"
}
