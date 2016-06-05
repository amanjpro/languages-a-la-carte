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

package ch.usi.inf.l3.sana.tiny.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.modifiers.Flags
import sana.tiny.names.Name
import sana.tiny.ast.{Attributes, noAttributes}


trait Symbol {
  var attributes: Attributes = noAttributes

  var name: Name
  var mods: Flags
  var tpe: Option[Type]
  var owner: Option[Symbol]

  protected[this] var decls: List[Symbol] = Nil

  def declarations: List[Symbol] = decls


  def declare(symbol: Symbol): Unit = decls = symbol :: decls

  def delete(symbol: Symbol): Unit = decls = decls.filter(_ != symbol)

  def directlyDefines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = decls.exists { s =>
      s == symbol && p(s)
    }

  def directlyDefinesName(name: Name,
    p: Symbol => Boolean): Boolean = decls.exists { sym =>
    sym.name == name && p(sym)
  }

  def getDirectlyDefinedSymbols(name: Name,
      p: Symbol => Boolean): List[Symbol] =
    decls.filter { sym =>
      sym.name == name && p(sym)
    }


  // Handling scoping, does this defines a symbol? If not see if
  // the owner defines it
  def defines(symbol: Symbol): Boolean =
    defines(symbol, _ => true)
  def defines(symbol: Symbol, p: Symbol => Boolean): Boolean =
    decls.exists(s => s == symbol && p(s)) || owner.map { sym =>
      sym.defines(symbol, p)
    }.getOrElse(false)


  def getDirectlyDefinedSymbol(name: Name,
          p: Symbol => Boolean): Option[Symbol] = {
    decls.find { sym =>
      sym.name == name && p(sym)
    }
  }
  // Handling scoping, does this defines a name with a predicate? If
  // not see if the owner defines it
  def getSymbol(name: Name, p: Symbol => Boolean): Option[Symbol] = {
    val thisSym = decls.find { sym =>
      sym.name == name && p(sym)
    }
    thisSym match {
      case None =>
        owner.flatMap { sym =>
          sym.getSymbol(name, p)
        }
      case _    => thisSym
    }
  }

}


trait TermSymbol extends Symbol
trait TypeSymbol extends Symbol
