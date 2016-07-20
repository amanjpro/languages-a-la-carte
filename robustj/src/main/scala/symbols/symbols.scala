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

package ch.usi.inf.l3.sana.robustj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.symbols.Symbol
import sana.tiny.modifiers.Flags
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.names.Name

object MethodSymbol {
  private class MethodSymbolImpl(var mods: Flags, var name: Name,
    var ret: Option[Symbol],
    var params: List[Symbol],
    var throwsSymbols: List[Symbol],
    var tpe: Option[Type],
    var owner: Option[Symbol]) extends MethodSymbol

  def apply(mods: Flags, name: Name, ret: Option[Symbol],
    params: List[Symbol], throwsSymbols: List[Symbol], tpe: Option[Type],
    owner: Option[Symbol]): MethodSymbol =
    new MethodSymbolImpl(mods, name, ret, params, throwsSymbols, tpe, owner)

  def unapply(sym: MethodSymbol):
    Option[(Flags, Name, Option[Symbol], List[Symbol], List[Symbol],
      Option[Type], Option[Symbol])] =
    sym match {
      case null    => None
      case _       =>
        Some((sym.mods, sym.name, sym.ret, sym.params, sym.throwsSymbols,
          sym.tpe, sym.owner))
    }
}

/** A trait for the symbols of methods */
trait MethodSymbol extends sana.primj.symbols.MethodSymbol {

  /** The list of all declared thrown exceptions for this method */
  var throwsSymbols: List[Symbol]

  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: MethodSymbol   =>
      this.throwsSymbols == that.throwsSymbols &&
        super.equals(that)
    case _                    =>
      false
  }

  override def toString(): String = s"Method symbol: $name"
  override def hashCode(): Int = name.hashCode * 43 + (41 * tpe.hashCode *
    ret.hashCode)
}
