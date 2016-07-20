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

package ch.usi.inf.l3.sana.ooj.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.ast.SimpleUseTree
import sana.tiny.symbols.Symbol



trait AugmentedSimpleUseTree
  extends sana.primj.ast.augmenters.AugmentedSimpleUseTree {

  /**
   * Returns the enclosing symbol of [[AugmentedSimpleUseTree.tree]].
   * SimpleUseTrees can have owner that are not enclosing them, for example
   * the owner of `id` in the expression `this.id` in the following example is
   * class `A`, but it is enclosed by method `m`.
   *
   * {{{
   * class A {
   *   int id = 1;
   *   void m() {
   *     this.id = 0;
   *   }
   * }
   * }}}
   */
  def enclosing: Option[Symbol] =
    tree.attributes.get('enclosing).map(_.asInstanceOf[Symbol])

  def enclosing_=(enclosing: Symbol): Unit =
    tree.attributes = tree.attributes + ('enclosing -> enclosing)

  /**
   * Returns true if [[AugmentedSimpleUseTree.tree]] is expected to be a static
   * tree. A tree is expected to be static if it is a part of a `SelectApi`,
   * and the `qual` part of the `SelectApi` points is a `TypeUseApi`.
   */
  def shouldBeStatic: Boolean =
    tree.attributes.get('shouldBeStatic)
      .map(_.asInstanceOf[Boolean]).getOrElse(false)

  def shouldBeStatic_=(shouldBeStatic: Boolean): Unit =
    tree.attributes = tree.attributes + ('shouldBeStatic -> shouldBeStatic)


  /**
   * Returns true if [[AugmentedSimpleUseTree.tree]] is part of a select
   * expression
   */
  def isQualified: Boolean =
    tree.attributes.get('isQualified)
      .map(_.asInstanceOf[Boolean]).getOrElse(false)

  def isQualified_=(isQualified: Boolean): Unit =
    tree.attributes = tree.attributes + ('isQualified -> isQualified)

}
