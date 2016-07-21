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

package ch.usi.inf.l3.sana.modulej.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj
import sana.ppj

import tiny.ast.{Tree, UseTree}
import tiny.names.Name
import ooj.ast.PackageDefApi



/********************* AST Nodes *********************************/

/** A tree to represent a compilation unit */
trait CompilationUnitApi extends ooj.ast.CompilationUnitApi {
  /** List of all import statements defined in this compilation tree */
  def imports: List[ImportApi]

  override def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = imports.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r2 = module.bottomUp(r1)(f)
    f(r2, this)
  }
}

/** A tree to represent an import statement */
trait ImportApi extends Tree {
  /** The qualified name (url) which is imported */
  def qual: UseTree

  /**
   * A flag to indicate if this import statement is an on-demand import
   * statement
   */
  def isOnDemand: Boolean


  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r = f(z, qual)
    f(r, this)
  }
}


protected[ast] class Import(val qual: UseTree,
    val isOnDemand: Boolean) extends ImportApi {
  override def toString: String = s"Import($qual, $isOnDemand)"
}

protected[ast] class CompilationUnit(val imports: List[ImportApi],
  val module: PackageDefApi,
  val sourceName: String,
  val sourcePath: List[String]) extends CompilationUnitApi {
  override def toString: String =
    s"CompilationUnit($imports, $module, $sourceName, $sourcePath)"
}
