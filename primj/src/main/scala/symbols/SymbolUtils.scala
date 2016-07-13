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

package ch.usi.inf.l3.sana.primj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.primj.types.VoidType

trait SymbolUtils extends sana.calcj.symbols.SymbolUtils {
  /**
   * Given a type, returns the symbol of this type if it is
   * primitive or void.
   *
   * @param t the type that we want to get its symbol
   * @return if the type is primitive or void, then its symbol or else None.
   */
  override def getSymbol(t: Type): Option[Symbol] = t match {
    case VoidType              => Some(VoidSymbol)
    case _                     => super.getSymbol(t)
  }

  override def standardDefinitions: Set[Symbol] =
    super.standardDefinitions + VoidSymbol

  /**
   * Given a symbol, returns its enclosing method if exists.
   */
  def enclosingMethod(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: MethodSymbol => Some(sym)
      case sym               => enclosingMethod(sym.owner)
    }


  /**
   * Checks if a local variable with the same owner is already defined in the given
   * owner scope and/or its owner scopes.
   *
   * @param owner the owner we want to examine.
   * @param name the potential name of the local variable(s).
   */
  def alreadyDefinedLocalVarable(owner: Option[Symbol],
    name: Name): Boolean = {
    owner match {
      case Some(owner: MethodSymbol)          =>
        owner.directlyDefinesName(name, _.isInstanceOf[VariableSymbol])
      case Some(owner: ScopeSymbol)           =>
        owner.directlyDefinesName(name, _.isInstanceOf[VariableSymbol]) ||
          alreadyDefinedLocalVarable(owner.owner, name)
      case _                                  =>
        false
    }
  }

}

object SymbolUtils extends SymbolUtils
