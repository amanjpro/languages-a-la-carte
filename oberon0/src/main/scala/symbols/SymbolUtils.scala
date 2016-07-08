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

package ch.usi.inf.l3.sana.oberon0.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.ooj.names.StdNames
import sana.ooj.modifiers.Ops._
import sana.ooj.types.TypeUtils
import sana.calcj.types.{IntType, BooleanType}
import sana.primj.types.VoidType
import sana.tiny.symbols.{TypeSymbol, TermSymbol}
import sana.primj.symbols.{ProgramSymbol, MethodSymbol,
                           VariableSymbol, ScopeSymbol,
                           VoidSymbol}

trait SymbolUtils extends sana.arrooj.symbols.SymbolUtils {

  def enclosingModule(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: ModuleSymbol  => Some(sym)
      case sym                => enclosingModule(sym.owner)
    }


  override def getSymbol(t: Type): Option[Symbol] = t match {
    case BooleanType           => Some(BooleanSymbol)
    case IntType               => Some(IntSymbol)
    case VoidType              => Some(VoidSymbol)
    case _                     => None
  }


  override def standardDefinitions: Set[Symbol] = Set(
      VoidSymbol,
      BooleanSymbol,
      IntSymbol
    )

}

object SymbolUtils extends SymbolUtils
