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

package ch.usi.inf.l3.sana.dcct.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.primj.types.VoidType
import sana.calcj.types.IntType
import sana.dcct.types.{CIntType, CSetType}
import sana.calcj.symbols.IntSymbol
import sana.ooj.symbols.ClassSymbol
import sana.tiny.symbols.{Symbol, TermSymbol, TypeSymbol}
import sana.tiny.modifiers.Flags
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.names.Name
import sana.tiny.names.StdNames.noname

trait IndexIntSymbol extends IntSymbol {
  override def name: Name = Name("Int")
}
object IndexIntSymbol extends IndexIntSymbol

trait CloudIntSymbol extends IntSymbol {
  override def name: Name = Name("CInt")
}
object CloudIntSymbol extends CloudIntSymbol


// TODO maybe there is a better base class to extend.
// TODO I am not sure about this overriding
trait CloudStringSymbol extends TypeSymbol {
  override def tpe: Option[Type] = Some(CIntType)
  override def owner: Option[Symbol] = None
  override def mods: Flags = noflags
  override def name: Name = Name("CString")

  override def mods_=(f:Flags): Unit = ???
  override def name_=(x: Name): Unit = ???
  override def owner_=(x:Option[Symbol]): Unit = ???
  override def tpe_=(x: Option[Type]): Unit = ???
  
  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}
object CloudStringSymbol extends CloudStringSymbol

// Will desugar to a java set later on I guess
trait CloudSetSymbol extends ClassSymbol {
  override def name: Name = Name("CSet")
  override def parents = ???
  override def mods = ???
  override def owner = ???
  override def tpe = Some(CSetType)

  override def parents_=(x: List[ClassSymbol]): Unit = ???
  override def mods_=(f:Flags): Unit = ???
  override def name_=(x: Name): Unit = ???
  override def owner_=(x:Option[Symbol]): Unit = ???
  override def tpe_=(x: Option[Type]): Unit = ???

}
object CloudSetSymbol extends CloudSetSymbol


trait ArraySymbol extends TypeSymbol {
  // TODO add suitable types.
  override def tpe: Option[Type] = None
  override def tpe_=(x: Option[Type]): Unit = ???
  override def mods: Flags = noflags
  override def mods_=(f:Flags): Unit = ???
  // TODO how can I get rid of this?
  override def name: Name = Name("Array")
  override def name_=(x: Name): Unit = ???
}

object ArraySymbol {
  private class ArraySymbolImp(override var name: Name,
        override var owner: Option[Symbol]) extends ArraySymbol 
  
  def apply(name: Name,
    owner: Option[Symbol]): ArraySymbol = new ArraySymbolImp(name, owner)
}

// TODO add a tuple type.
