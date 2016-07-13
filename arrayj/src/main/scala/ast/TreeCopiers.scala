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
import sana.brokenj

import tiny.symbols.Symbol
import tiny.types.Type
import tiny.ast.{Tree, Expr, UseTree}
import tiny.source.Position

trait TreeCopiers extends brokenj.ast.TreeCopiers {

  /**
   * Returns a copy of an array initialization expression
   *
   * @param template the tree to be copied
   * @param elements the list of elements of this initialization
   */
  def copyArrayInitializer(template: ArrayInitializerApi)(
      elements: List[Expr] = template.elements): ArrayInitializerApi = {
    val res = TreeFactories.mkArrayInitializer(elements)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of an array access expression
   *
   * @param template the tree to be copied
   * @param array the array to be accessed
   * @param index the index of the element that is accessed
   */
  def copyArrayAccess(template: ArrayAccessApi)(
    array: Expr = template.array,
    index: Expr = template.index): ArrayAccessApi = {
    val res = TreeFactories.mkArrayAccess(array, index)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of an array access-type-use
   *
   * @param template the tree to be copied
   * @param array the type-tree of the array
   */
  def copyArrayTypeUse(template: ArrayTypeUseApi)(
      tpt: UseTree = template.tpt): ArrayTypeUseApi = {
    val res = TreeFactories.mkArrayTypeUse(tpt)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of an array access-creation expression
   *
   * @param template the tree to be copied
   * @param array the expression that represents the array that is created
   * @param size the size of the array to be created
   */
  def copyArrayCreation(template: ArrayCreationApi)(
      array: Expr = template.array,
      size: Option[Expr] = template.size): ArrayCreationApi = {
    val res = TreeFactories.mkArrayCreation(array, size)
    copyProperties(template, res)
    res
  }
}


object TreeCopiers extends TreeCopiers
