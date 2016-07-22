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
import tiny.types.Type
import tiny.modifiers.Ops.noflags
import primj.ast.{BlockApi}
import tiny.names.Name
import tiny.names.StdNames._
import tiny.symbols.Symbol
import tiny.source.Position
import arrayj.ast.{TreeFactories => ATreeFactories, _}
import ooj.ast.{TreeFactories => OTreeFactories, _}


trait TreeFactories extends primj.ast.TreeFactories {
  /**
   * Creates a new tree to represent a module definition in a source file.
   *
   * @param name the name of this module
   * @param declarations the list of the members of this module
   * @param block the body of this module
   * @param pos the position of this module
   * @param symbol the symbol of this module
   * @param owner the owner of this module
   */
  def mkModuleDef(name: Name, declarations: List[DefTree],
    block: Option[BlockApi], pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): ModuleDefApi = {

    val res = new ModuleDef(name, declarations, block)
    pos.foreach(res.pos = _)
    symbol.foreach(res.symbol = _)
    owner.foreach(res.owner = _)
    res
  }


  /**
   * Creates a new tree for array-type-use expressions
   *
   * @param name the name of this type-alias
   * @param tpt the type tree that this alias points to
   * @param pos the position of this type-alias
   * @param symbol the symbol of this type-alias
   * @param owner the owner of this type-alias
   */
  def mkTypeDef(name: Name, tpt: Tree,
    pos: Option[Position] = None, symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): TypeDefApi = {

    val res = new TypeDef(name, tpt)
    pos.foreach(res.pos = _)
    symbol.foreach { s =>
      s.tpe.foreach(res.tpe = _)
      res.symbol = s
    }
    owner.foreach(res.owner = _)
    res
  }

  /**
   * Creates a new tree for array-type-use expressions
   *
   * @param tpt the type-tree of this array type tree
   * @param size the size of this array type tree
   * @param pos the position of this array type tree
   * @param symbol the symbol of this array type tree
   * @param owner the owner of this array type tree
   * @param tpe the type-information of this array type tree
   */
  def mkArrayTypeUse(tpt: UseTree,
    size: Expr,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayTypeUseApi = {
    val res = new ArrayTypeUse(tpt, size)
    pos.foreach(res.pos = _)
    symbol.foreach(res.symbol = _)
    owner.foreach(res.owner = _)
    tpe.foreach(res.tpe = _)
    res
  }




  /**
   * Creates a new tree to represent a record definition
   *
   * @param body the body of this class
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   * @param tpe the type of this tree
   */
  def mkRecordDef(body: TemplateApi,
      pos: Option[Position] = None,
      symbol: Option[Symbol] = None,
      tpe: Option[Type] = None): ClassDefApi =
    OTreeFactories.mkClassDef(noflags, noname, Nil, body, pos, symbol, tpe)

  /**
   * Creates a new tree to represent the body of a record definition
   *
   * @param members the list of the members of this class-body
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def mkTemplate(members: List[Tree],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): TemplateApi =
    OTreeFactories.mkTemplate(members, pos, owner)

  /**
   * Creates an instance of a member selection tree
   *
   * @param qual the tree that has been selected from
   * @param tree the tree that has been selected
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   * @param owner the owner of this tree
   */
  def mkSelect(qual: Tree, tree: SimpleUseTree,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): SelectApi =
    OTreeFactories.mkSelect(qual, tree, pos, symbol, owner)

  /**
   * Creates a new tree for array-access expressions
   *
   * @param array the array to be accessed
   * @param index the index of the element that is accessed
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   * @param owner the owner of this tree
   * @param tpe the type-information of this tree
   */
  def mkArrayAccess(array: Expr, index: Expr,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayAccessApi =
    ATreeFactories.mkArrayAccess(array, index, pos, symbol, owner, tpe)

}

object TreeFactories extends TreeFactories
