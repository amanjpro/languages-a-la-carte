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

package ch.usi.inf.l3.sana.robustj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj
import sana.robustj

import tiny.ast.{Tree, Expr, UseTree}
import primj.ast.{ValDefApi, BlockApi}
import tiny.names.Name
import tiny.modifiers.Flags



/********************* AST Nodes *********************************/

trait ThrowApi extends Expr {
  def expr: Expr


  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, expr)
    f(r1, this)
  }
}


trait TryApi extends Expr {
  def tryClause: BlockApi
  def catches: List[CatchApi]
  def finallyClause: Option[BlockApi]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, tryClause)
    val r2 = catches.foldLeft(r1)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r3 = finallyClause.map(fc => f(r2, fc)).getOrElse(r2)
    f(r3, this)
  }
}


trait CatchApi extends Expr {
  def eparam: ValDefApi
  def catchClause: BlockApi

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, eparam)
    val r2 = f(r1, catchClause)
    f(r2, this)
  }
}

trait MethodDefApi extends ooj.ast.MethodDefApi {
  def throwsClause: List[UseTree]
}

protected[ast] class Throw(val expr: Expr) extends ThrowApi {
  override def toString: String = s"Throw($expr)"
}


protected[ast] class Try(val tryClause: BlockApi,
        val catches: List[CatchApi],
        val finallyClause: Option[BlockApi]) extends TryApi {
  override def toString: String =
    s"Try($tryClause, $catches, $finallyClause)"
}


protected[ast] class Catch(val eparam: ValDefApi,
        val catchClause: BlockApi) extends CatchApi {
  override def toString: String =
    s"Catch($eparam, $catchClause)"
}

protected[ast] class MethodDef(val mods: Flags,
  val ret: UseTree,
  val name: Name, val params: List[ValDefApi],
  val throwsClause: List[UseTree],
  val body: Expr) extends MethodDefApi {
  override def toString: String =
    s"MethodDef($mods, $ret, $name, $params, $throwsClause, $body)"
}
