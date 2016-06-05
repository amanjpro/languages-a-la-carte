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

package ch.usi.inf.l3.sana.modulej.symbols

import ch.usi.inf.l3.sana

import sana.ooj
import sana.tiny

import tiny.symbols.Symbol

object CompilationUnitSymbol {
  private class CompilationUnitSymbolImpl(
    var importURIs: List[(Symbol, String)],
    var module: Option[Symbol],
    var sourceName: String, var sourcePath: List[String],
    var owner: Option[Symbol]) extends CompilationUnitSymbol

  def apply(importURIs: List[(Symbol, String)],
    module: Option[Symbol], sourceName: String,
    sourcePath: List[String], owner: Option[Symbol]): CompilationUnitSymbol =
      new CompilationUnitSymbolImpl(importURIs, module,
        sourceName, sourcePath, owner)


  def unapply(sym: CompilationUnitSymbol):
    Option[(List[(Symbol, String)], Option[Symbol], String,
      List[String], Option[Symbol])] =
    sym match {
      case null                    => None
      case _                       =>
        Some((sym.importURIs, sym.module, sym.sourceName,
          sym.sourcePath, sym.owner))
    }
}


trait CompilationUnitSymbol extends ooj.symbols.CompilationUnitSymbol {
  // String is fully qualified uri for this import statement
  var importURIs: List[(Symbol, String)]
}
