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

package ch.usi.inf.l3.sana.calcj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.{TypeSymbol, Symbol}
import sana.tiny.types.Type
import sana.calcj.types._
import sana.tiny.modifiers.Flags
import sana.tiny.modifiers.Ops._
import sana.tiny.names.Name

trait IntSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(IntType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("int")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???



  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object IntSymbol extends IntSymbol

trait CharSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(CharType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("char")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object CharSymbol extends CharSymbol

trait ShortSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(ShortType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("short")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object ShortSymbol extends ShortSymbol

trait ByteSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(ByteType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("byte")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object ByteSymbol extends ByteSymbol

trait LongSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(LongType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("long")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object LongSymbol extends LongSymbol

trait FloatSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(FloatType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("float")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object FloatSymbol extends FloatSymbol

trait DoubleSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(DoubleType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("double")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object DoubleSymbol extends DoubleSymbol


trait BooleanSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(BooleanType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("boolean")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object BooleanSymbol extends BooleanSymbol
