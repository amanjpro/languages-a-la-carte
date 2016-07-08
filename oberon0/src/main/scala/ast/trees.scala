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

package ch.usi.inf.l3.sana.oberon0.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.arrayj

import tiny.ast.{Tree, DefTree, TypeTree, TermTree, UseTree, Expr}
import primj.ast.{BlockApi}
import tiny.names.Name

trait ModuleDefApi extends TermTree {
  def name: Name
  def declarations: List[DefTree]
  def block: Option[BlockApi]


  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = declarations.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r2 = block.map(b => f(r1, b)).getOrElse(r1)
    f(r1, this)
  }
}

trait ArrayTypeUseApi extends arrayj.ast.ArrayTypeUseApi {
  def size: Expr

  override def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, tpt)
    val r2 = f(z, size)
    f(r2, this)
  }
}

trait TypeDefApi extends TypeTree {
  def name: Name
  def tpt: Tree

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, tpt)
    f(r1, this)
  }
}




private[this] class ModuleDef(val name: Name,
  val declarations: List[DefTree],
  val block: Option[BlockApi]) extends ModuleDefApi {
  override def toString: String = s"ModuleDef($name, $declarations, $block)"
}

private[this] class TypeDef(val name: Name,
  val tpt: Tree) extends TypeDefApi {
  override def toString: String = s"TypeDef($name, $tpt)"
}


protected[ast] class ArrayTypeUse(val tpt: UseTree,
    val size: Expr) extends ArrayTypeUseApi {
  override def toString: String = s"ArrayTypeUse($tpt, $size)"
}
