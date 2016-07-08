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

package ch.usi.inf.l3.sana.guod.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.calcj.types.IntType
import sana.calcj.symbols._
import sana.primj.symbols.{VariableSymbol, VoidSymbol}
import sana.ooj.symbols.{ClassSymbol, PackageSymbol, CompilationUnitSymbol}
import sana.ooj.modifiers._
import sana.robustj.names.StdNames

import sana.ooj.symbols.PackageSymbol

trait SymbolUtils extends sana.modulej.symbols.SymbolUtils {
  def toFullyQualifiedTypeName(symbol: Option[Symbol]): String = symbol match {
    case Some(vsym: VariableSymbol)         =>
      toFullyQualifiedTypeName(vsym.typeSymbol)
    case Some(csym: ClassSymbol)            =>
      toFullyQualifiedTypeName(csym.owner) + csym.name
    case Some(pkg: PackageSymbol)           =>
      if(pkg.name != StdNames.DEFAULT_PACKAGE_NAME)
        toFullyQualifiedTypeName(pkg.owner) + pkg.name + "."
      else ""
    case Some(cunit: CompilationUnitSymbol) =>
      toFullyQualifiedTypeName(cunit.owner)
    case Some(BooleanSymbol)                =>
      BooleanSymbol.name.asString
    case Some(ByteSymbol)                   =>
      ByteSymbol.name.asString
    case Some(CharSymbol)                   =>
      CharSymbol.name.asString
    case Some(ShortSymbol)                  =>
      ShortSymbol.name.asString
    case Some(IntSymbol)                    =>
      IntSymbol.name.asString
    case Some(LongSymbol)                   =>
      LongSymbol.name.asString
    case Some(FloatSymbol)                  =>
      FloatSymbol.name.asString
    case Some(DoubleSymbol)                 =>
      DoubleSymbol.name.asString
    case Some(VoidSymbol)                   =>
      VoidSymbol.name.asString
    case t                                  =>
      ""
  }
}

object SymbolUtils extends SymbolUtils
