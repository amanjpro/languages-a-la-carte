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

package ch.usi.inf.l3.sana.arrooj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.modifiers.Flags
import sana.tiny.modifiers.Ops._
import sana.tiny.names.Name
import sana.tiny.symbols.{Symbol, TypeSymbol}
import sana.arrooj.types.{ArrayType, TypeUtils}
import sana.ooj.symbols.ClassSymbol



trait ArraySymbol extends ClassSymbol {
  var componentSymbol: Symbol
  def objectClassSymbol: ClassSymbol
  def members: List[Symbol]


  decls = decls ++ members

  def parents: List[ClassSymbol] = List(objectClassSymbol)
  def parents_=(parents: List[ClassSymbol]): Unit = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???

  def owner: Option[Symbol] = componentSymbol.owner
  def owner_=(onwer: Option[Symbol]): Unit = ???

  def name: Name = componentSymbol.name
  def name_=(name: Name): Unit = ???

  def mods: Flags = noflags
  def mods_=(mods: Flags): Unit = ???

  def tpe: Option[Type] = for {
    ctpe <- componentSymbol.tpe
    otpe <- objectClassSymbol.tpe
  } yield TypeUtils.mkArrayType(ctpe)
  def tpe_=(tpe: Option[Type]): Unit = ???


  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: ArraySymbol    =>
      this.owner == that.owner &&
        this.componentSymbol == that.componentSymbol &&
        this.name == that.name
    case _                    =>
      false
  }
  override def toString(): String = s"Array symbol: $name"
  override def hashCode(): Int = name.hashCode * 43 + componentSymbol.hashCode
}


class ArraySymbolImpl(var componentSymbol: Symbol,
    val objectClassSymbol: ClassSymbol,
    val members: List[Symbol]) extends ArraySymbol
