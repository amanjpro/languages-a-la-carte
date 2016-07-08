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

package ch.usi.inf.l3.sana.tiny.ast

import ch.usi.inf.l3.sana
// import sana.core.SyntaxComponent
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.StdNames._
import sana.tiny.names.Name


/**
 * The supertype of all trees.
 */
trait Tree {
  /**
   * The attributes of a tree. This is to implement open-classes in
   * our framework (and Scala).
   */
  var attributes: Attributes = noAttributes

  /**
   * Reduces this tree using a given function in a bottom-up manner.
   *
   * @param z the starting value
   * @param f the function to be used for reducing this tree.
   */
  def bottomUp[R](z: R)(f: (R, Tree) => R): R

  /**
   * Applies a function on this tree and all its children, in a bottom-up manner.
   *
   * @param f the function to be applied
   */
  def foreach(f: Tree => Unit): Unit = {
    bottomUp(())((z, y) => f(y))
  }
}




/**
 * The supertype of all trees that have a name, trees can have a name that they
 * do not define, e.g. identifiers.
 */
trait NamedTree extends Tree {
  def name: Name
}

/** The supertype of all trees that define a name */
trait DefTree extends NamedTree


/** The supertype of all trees that represent a term, like fields and methods */
trait TermTree extends DefTree

/** The supertype of all trees that represent a type, like classes */
trait TypeTree extends DefTree

/** The supertype of all trees that represent an expression, like binary operations */
trait Expr extends Tree

/** The supertype of all trees that use a defined name, like identifiers */
trait UseTree extends Expr with NamedTree

/**
 * The supertype of all trees that use a defined name, like identifiers. The difference
 * between this and {{{UseTree}}} is that {{{SimpleUseTree}}} do not represent the
 * compound trees, like {{{selector.selected}}}.
 **/
trait SimpleUseTree extends UseTree


/** This tree represents a tree that points to a defined type. */
trait TypeUseApi extends SimpleUseTree {
  def bottomUp[R](z: R)(f: (R, Tree) => R): R = f(z, this)
}

/** This tree represents a tree that points to a defined term. */
trait IdentApi extends SimpleUseTree {
  def bottomUp[R](z: R)(f: (R, Tree) => R): R = f(z, this)
}

protected[ast] class TypeUse(val name: Name) extends TypeUseApi {
  override def toString: String = s"TypeUse($name)"
}

protected[ast] class Ident(val name: Name) extends IdentApi {
  override def toString: String = s"Ident($name)"
}

case object NoTree extends Expr {
  def bottomUp[R](z: R)(f: (R, Tree) => R): R = f(z, this)
}


case object ErrorTree extends Tree {
  def bottomUp[R](z: R)(f: (R, Tree) => R): R = f(z, this)
}
