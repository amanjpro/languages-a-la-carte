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


/**
 * The supertype of all symbols. A symbol is attached to an AST and stores its
 * information. Symbols are used for name resolution.
 */
trait Symbol {
  /**
   * The attributes of this symbol. This is to implement open-classes in
   * our framework (and Scala).
   */
  var attributes: Attributes = noAttributes

  /**
   * The name of this symbol, it should be the same as the name of the
   * tree that it represents.
   */
  var name: Name

  /**
   * The modifiers of this symbol, it should be the same as the name of the
   * tree that it represents.
   */
  var mods: Flags

  /**
   * The type of this symbol
   */
  var tpe: Option[Type]

  /**
   * The contextual owner of this symbol. In the following example:
   * {{{
   * class A {
   *   int f = 0;
   *   void m(int a) {
   *     int d = a;
   *     f = d;
   *     while(true) {
   *       int k = f;
   *     }
   *   }
   * }
   * }}}
   * The owner of field f and method m is the class A, and the owner of
   * method m; And the owner of parameter a and variable d and the while
   * loop is the method m. Lastly, the owner of the variable k is the
   * while loop.
   */
  var owner: Option[Symbol]

  /**
   * The members of this symbol, namely the names that are directly defined inside
   * the tree that this symbol is attached to.
   */
  protected[this] var decls: List[Symbol] = Nil

  /**
   * @see [[ch.usi.inf.l3.sana.tiny.symbols.Symbol.decls]]
   */
  def declarations: List[Symbol] = decls


  /**
   * Declares a member of this symbol.
   *
   * @param symbol the symbol to be declared.
   */
  def declare(symbol: Symbol): Unit = decls = symbol :: decls

  /**
   * Deletes a member of this symbol.
   *
   * @param symbol the symbol to be deleted.
   */
  def delete(symbol: Symbol): Unit = decls = decls.filter(_ != symbol)

  /**
   * Checks if the passed symbol is declared as the direct child of
   * this symbol and satisfies a predicate.
   *
   * @param symbol the symbol to be checked
   * @param p the predicate
   */
  def directlyDefines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = decls.exists { s =>
      s == symbol && p(s)
    }

  /**
   * Checks if there is a symbol that has the given name and is directly
   * defined as the child of this symbol and satisfies a predicate.
   *
   * @param name the name of the symbol to be checked
   * @param p the predicate
   */
  def directlyDefinesName(name: Name,
    p: Symbol => Boolean): Boolean = decls.exists { sym =>
    sym.name == name && p(sym)
  }

  /**
   * Returns a list of symbols that have the given name and are directly
   * defined as the children of this symbol and satisfy a predicate.
   *
   * @param name the name of the symbols
   * @param p the predicate
   */
  def getDirectlyDefinedSymbols(name: Name,
      p: Symbol => Boolean): List[Symbol] =
    decls.filter { sym =>
      sym.name == name && p(sym)
    }

  /**
   * Checks if the passed symbol accessible from this symbol.
   *
   * @param symbol the symbol to be checked
   */
  def defines(symbol: Symbol): Boolean = {
    // Handling scoping, does this defines a symbol? If not see if
    // the owner defines it
    defines(symbol, _ => true)
  }

  /**
   * Checks if the passed symbol accessible from this symbol and
   * satisfies a predciate
   *
   * @param symbol the symbol to be checked
   * @param p the predicate
   */
  def defines(symbol: Symbol, p: Symbol => Boolean): Boolean =
    decls.exists(s => s == symbol && p(s)) || owner.map { sym =>
      sym.defines(symbol, p)
    }.getOrElse(false)

  /**
   * Optionally returns a symbol that has the given name and is directly
   * defined as the child of this symbol and satisfies a predicate.
   *
   * @param name the name of the symbol to be checked
   * @param p the predicate
   */
  def getDirectlyDefinedSymbol(name: Name,
          p: Symbol => Boolean): Option[Symbol] = {
    decls.find { sym =>
      sym.name == name && p(sym)
    }
  }

  /**
   * Optionally returns a symbol that has the given name and is
   * accessible from this symbol and satisfies a predicate.
   *
   * @param name the name of the symbol to be checked
   * @param p the predicate
   */
  def getSymbol(name: Name, p: Symbol => Boolean): Option[Symbol] = {
    // Handling scoping, does this defines a name with a predicate? If
    // not see if the owner defines it
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


/**
 * The supertype of all term-symbol. Term-symbols are attached to the
 * trees that do not introduce a new type, but a new term like fields,
 * packages and methods.
 */
trait TermSymbol extends Symbol

/**
 * The supertype of all type-symbol. Type-symbols are attached to the
 * trees that introduce a new type, like classes and interfaces.
 */

trait TypeSymbol extends Symbol
