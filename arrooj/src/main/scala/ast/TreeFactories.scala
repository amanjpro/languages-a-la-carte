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

package ch.usi.inf.l3.sana.arrooj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.brokenj
import sana.ooj
import sana.arrayj

import tiny.symbols.Symbol
import tiny.types.Type
import tiny.ast.{Tree, Expr, UseTree}
import tiny.source.Position
import arrayj.ast.{TreeFactories => ATreeFactories, _}

trait TreeFactories extends ooj.ast.TreeFactories {
  /** @see {{{arrayj.ast.TreeFactories.mkArrayInitializer}}} */
  def mkArrayInitializer(elements: List[Expr],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayInitializerApi =
    ATreeFactories.mkArrayInitializer(elements, pos, symbol, owner, tpe)


  /** @see {{{arrayj.ast.TreeFactories.mkArrayAccess}}} */
  def mkArrayAccess(array: Expr, index: Expr,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayAccessApi =
    ATreeFactories.mkArrayAccess(array, index, pos, symbol, owner, tpe)


  /** @see {{{arrayj.ast.TreeFactories.mkArrayTypeUse}}} */
  def mkArrayTypeUse(tpt: UseTree,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayTypeUseApi =
    ATreeFactories.mkArrayTypeUse(tpt, pos, symbol, owner, tpe)

  /** @see {{{arrayj.ast.TreeFactories.mkArrayCreation}}} */
  def mkArrayCreation(array: Expr,
    size: Option[Expr],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayCreationApi =
    ATreeFactories.mkArrayCreation(array, size, pos, symbol, owner, tpe)
}

object TreeFactories extends TreeFactories
