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

package ch.usi.inf.l3.sana.oberon0.symbols

import ch.usi.inf.l3.sana
import sana.tiny
import sana.ooj
import sana.calcj


import tiny.names.Name
import tiny.modifiers.Flags
import tiny.modifiers.Ops.noflags
import tiny.types.Type
import tiny.symbols.{Symbol, TermSymbol, TypeSymbol}
import ooj.symbols.PackageSymbol

trait IntSymbol extends calcj.symbols.IntSymbol {
  override def name: Name = Name("INTEGER")
}

case object IntSymbol extends IntSymbol

trait BooleanSymbol extends calcj.symbols.BooleanSymbol {
  override def name: Name = Name("BOOLEAN")
}

case object BooleanSymbol extends BooleanSymbol


object ModuleSymbol {
  private class ModuleSymbolImpl(var name: Name,
    var owner: Option[Symbol]) extends ModuleSymbol

  def apply(name: Name, owner: Option[Symbol]): ModuleSymbol =
    new ModuleSymbolImpl(name, owner)


  def unapply(sym: ModuleSymbol): Option[(Name, Option[Symbol])] = sym match {
    case null              => None
    case _                 => Some((sym.name, sym.owner))
  }
}

trait ModuleSymbol extends PackageSymbol {
  override def qualifiedName: String = name.asString

  override def qualifiedNameAsList: List[Name] = List(name)

  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: ModuleSymbol   =>
      this.name == that.name
    case _                    =>
      false
  }

  override def toString(): String = s"Module symbol: $name"
  override def hashCode(): Int = name.hashCode * 43
}


trait TypeDefSymbol extends TypeSymbol {
  def mods: Flags = noflags
  def mods_=(flags: Flags): Unit = ???

  var name: Name
  var typeSymbol: Option[Symbol]
  var owner: Option[Symbol]

  def tpe: Option[Type] = typeSymbol.flatMap(_.tpe)
  def tpe_=(tpe: Option[Type]): Unit = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean =
    typeSymbol.map(_.defines(symbol, p)).getOrElse(false)

  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = {
    typeSymbol.flatMap(_.getSymbol(name, p))
  }

  override def equals(other: Any): Boolean = other match {
    case null                    => false
    case that: TypeDefSymbol =>
        this.name == that.name &&
        this.owner == that.owner &&
        this.typeSymbol == that.typeSymbol
    case _                    =>
      false
  }
  override def toString(): String = s"TypeDef symbol $name"
  override def hashCode(): Int = name.hashCode * 43 +
    typeSymbol.hashCode * mods.hashCode + 47 * owner.hashCode
}


object TypeDefSymbol {
  private class TypeDefSymbolImpl(var name: Name,
    var typeSymbol: Option[Symbol], var owner: Option[Symbol])
    extends TypeDefSymbol

  def apply(name: Name, typeSymbol: Option[Symbol],
      owner: Option[Symbol]): TypeDefSymbol =
        new TypeDefSymbolImpl(name, typeSymbol, owner)

  def unapply(sym: TypeDefSymbol):
    Option[(Name, Option[Symbol], Option[Symbol])] = sym match {
      case null      => None
      case _         => Some((sym.name, sym.typeSymbol, sym.owner))
    }
}
