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
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.calcj.types.IntType
import sana.ooj.symbols.ClassSymbol
import sana.ooj.modifiers._
import sana.robustj.names.StdNames

trait SymbolUtils extends sana.arrooj.symbols.SymbolUtils {

  /** The symbol for {{{java.lang.Throwable}}} class */
  lazy val throwableClassSymbol: ClassSymbol = {
    val name    = StdNames.THROWABLE_CLASS_NAME
    langPackageSymbol.getSymbol(name, _ => true).get.asInstanceOf[ClassSymbol]
  }

  /** The symbol for {{{java.lang.Exception}}} class */
  lazy val exceptionClassSymbol: ClassSymbol = {
    val name    = StdNames.EXCEPTION_CLASS_NAME
    langPackageSymbol.getSymbol(name, _ => true).get.asInstanceOf[ClassSymbol]
  }

  /** The symbol for {{{java.lang.RuntimeException}}} class */
  lazy val runtimeExceptionClassSymbol: ClassSymbol = {
    val name    = StdNames.RUNTIME_EXCEPTION_CLASS_NAME
    langPackageSymbol.getSymbol(name, _ => true).get.asInstanceOf[ClassSymbol]
  }

  /** The symbol for {{{java.lang.Error}}} class */
  lazy val errorClassSymbol: ClassSymbol = {
    val name    = StdNames.ERROR_CLASS_NAME
    langPackageSymbol.getSymbol(name, _ => true).get.asInstanceOf[ClassSymbol]
  }
}

object SymbolUtils extends SymbolUtils
