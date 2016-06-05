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

package ch.usi.inf.l3.sana.dcct.ast

import ch.usi.inf.l3.sana
import sana.primj._
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.primj.ast.Implicits._
import sana.primj.types.MethodType
import sana.tiny.modifiers.Flags

import sana.tiny.ast._
import sana.calcj.ast._
import sana.primj.ast._


trait TreeFactories extends sana.ooj.ast.TreeFactories {
  // TODO Copied from primj factories. Is there a nicer way to do it?
  def mkActionDef(ret: UseTree,
    name: Name, params: List[ValDefApi],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): MethodDefApi = {
    val res = new MethodDef(ret, name, params, body)
    pos.foreach(res.pos = _)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    val tys = params.flatMap(_.tpe)
    ret.tpe.foreach(t => res.tpe = MethodType(t, tys))
    res
  }

  
  def mkArrayDef(name: Name, indices: List[ValDefApi], 
      properties: List[ValDefApi], symbol: Option[Symbol] = None) : ArrayDefApi = {
    val res = new ArrayDef(name, indices, properties)
    symbol.foreach ( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    res
  }
 
  def mkForEach(entityVar: ValDefApi, whereExpr: Expr, body: BlockApi,
    symbol: Option[Symbol] = None): ForEach = {
    val res = new ForEach(entityVar, whereExpr, body)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    res
  }     
}

object TreeFactories extends TreeFactories
