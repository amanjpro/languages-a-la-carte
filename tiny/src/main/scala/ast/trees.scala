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

package ch.usi.inf.l3.sana.tiny.ast

import ch.usi.inf.l3.sana
// import sana.core.SyntaxComponent
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.StdNames._
import sana.tiny.names.Name



trait Tree {
  var attributes: Attributes = noAttributes

  def bottomUp[R](z: R)(f: (R, Tree) => R): R
  def foreach(f: Tree => Unit): Unit = {
    bottomUp(())((z, y) => f(y))
  }
}




trait NamedTree extends Tree {
  def name: Name
}

trait DefTree extends NamedTree


trait TermTree extends DefTree

trait TypeTree extends DefTree

trait Expr extends Tree

trait UseTree extends Expr with NamedTree

trait SimpleUseTree extends UseTree


trait TypeUseApi extends SimpleUseTree {
  def bottomUp[R](z: R)(f: (R, Tree) => R): R = f(z, this)
}
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
