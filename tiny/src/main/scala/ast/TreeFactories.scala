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

package ch.usi.inf.l3.sana.tiny.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import Implicits._


/**
 * A factory for initializing new trees.
 */
trait TreeFactories {

  /**
   * Creates a new identifier
   *
   * @param name the name of the identifier
   * @param pos the position of the identifier
   * @param symbol the symbol of the identifier
   * @param owner the owner of the identifier
   */
  def mkIdent(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            owner: Option[Symbol] = None): IdentApi = {
    val res = new Ident(name)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    symbol.foreach(sym => {
      res.symbol = sym
      sym.tpe.foreach(res.tpe = _)
    })
    res
  }

  /**
   * Creates a new type-use
   *
   * @param name the name of the type-use
   * @param pos the position of the type-use
   * @param symbol the symbol of the type-use
   * @param owner the owner of the type-use
   * @see [[ch.usi.inf.l3.sana.tiny.ast.TypeUse]]
   */
  def mkTypeUse(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            owner: Option[Symbol] = None): TypeUseApi = {
    val res = new TypeUse(name)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    symbol.foreach(sym => {
      res.symbol = sym
      sym.tpe.foreach(res.tpe = _)
    })
    res
  }
}


object TreeFactories extends TreeFactories
