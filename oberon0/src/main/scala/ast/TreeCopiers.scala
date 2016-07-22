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
import sana.ooj

import tiny.ast._
import tiny.ast.Implicits._
import primj.ast.{BlockApi}
import tiny.names.Name
import tiny.symbols.Symbol
import tiny.source.Position
import arrayj.ast.{TreeCopiers => ATreeCopiers, _}
import ooj.ast.{TreeCopiers => OTreeCopiers, _}


trait TreeCopiers extends primj.ast.TreeCopiers {

  /**
   * Returns a copy of a module
   *
   * @param template the tree to be copied
   * @param name the name of this module
   * @param declarations the declarations of this module
   * @param block the statements of this module
   */
  def copyModuleDef(template: ModuleDefApi)(name: Name = template.name,
    declarations: List[DefTree] = template.declarations,
    block: Option[BlockApi] = template.block): ModuleDefApi = {
    val res = TreeFactories.mkModuleDef(name, declarations, block)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a type-alias
   *
   * @param template the tree to be copied
   * @param name the name of this type-alias
   * @param tpt the type-tree of this type-alias
   */
  def copyTypeDef(template: TypeDefApi)(name: Name = template.name,
    tpt: Tree = template.tpt): TypeDefApi = {

    val res = TreeFactories.mkTypeDef(name, tpt)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of an array access-type-use
   *
   * @param template the tree to be copied
   * @param tpt the type-tree of the array
   * @param size the size of the array
   */
  def copyArrayTypeUse(template: ArrayTypeUseApi)(
      tpt: UseTree = template.tpt,
      size: Expr  = template.size): ArrayTypeUseApi = {
    val res = TreeFactories.mkArrayTypeUse(tpt, size)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a record definition
   *
   * @param template the tree to be copied
   * @param body the body of this record definition
   */
  def copyRecordDef(template: ClassDefApi)(
      body: TemplateApi = template.body): ClassDefApi =
    OTreeCopiers.copyClassDef(template)(template.mods,
      template.name, template.parents, body)

  /**
   * Returns a copy of a body of a record
   *
   * @param template the tree to be copied
   * @param members the members of the body of the record
   */
  def copyTemplate(template: TemplateApi)(
      members: List[Tree] = template.members): TemplateApi =
    OTreeCopiers.copyTemplate(template)(members)

  /**
   * Returns a copy of a `select` expression
   *
   * @param template the tree to be copied
   * @param qual the tree that has been selected from
   * @param tree the tree that has been selected
   */
  def copySelect(template: SelectApi)(qual: Tree = template.qual,
    tree: SimpleUseTree = template.tree): SelectApi =
    OTreeCopiers.copySelect(template)(qual, tree)


  /**
   * Returns a copy of an array access expression
   *
   * @param template the tree to be copied
   * @param array the array to be accessed
   * @param index the index of the element that is accessed
   */
  def copyArrayAccess(template: ArrayAccessApi)(
    array: Expr = template.array,
    index: Expr = template.index): ArrayAccessApi =
    ATreeCopiers.copyArrayAccess(template)(array, index)



}

object TreeCopiers extends TreeCopiers
