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

package ch.usi.inf.l3.sana.tiny.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.ast.Tree
import sana.tiny.symbols.Symbol
import sana.tiny.source.Position



trait AugmentedTree {
  /** The tree to be augmented */
  val tree: Tree

  /** Returns the type of [[AugmentedTree.tree]] */
  def tpe: Option[Type] = {
    tree.attributes.get('type).map(_.asInstanceOf[Type])
  }

  def tpe_=(tpe: Type): Unit = {
    tree.attributes = tree.attributes + ('type -> tpe)
  }

  /** Returns the owner of [[AugmentedTree.tree]] */
  def owner: Option[Symbol] = {
    tree.attributes.get('owner).map(_.asInstanceOf[Symbol])
  }

  def owner_=(owner: Symbol): Unit = {
    tree.attributes = tree.attributes + ('owner -> owner)
  }

  /** Returns the symbol of [[AugmentedTree.tree]] */
  def symbol: Option[Symbol] = {
    tree.attributes.get('symbol).map(_.asInstanceOf[Symbol])
  }

  def symbol_=(sym: Symbol): Unit = {
    tree.attributes = tree.attributes + ('symbol -> sym)
  }

  /** Returns the position of [[AugmentedTree.tree]] */
  def pos: Option[Position] = {
    tree.attributes.get('position).map(_.asInstanceOf[Position])
  }

  def pos_=(pos: Position): Unit = {
    tree.attributes = tree.attributes + ('position -> pos)
  }
}
