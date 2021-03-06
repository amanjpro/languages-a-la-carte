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

package ch.usi.inf.l3.sana.arrooj.types


import ch.usi.inf.l3.sana

import sana.tiny
import sana.ooj

import tiny.types.Type
import ooj.types.RefType
import tiny.names.Name
import tiny.symbols.Symbol

/**
 * A type for arrays
 *
 * The difference between this and [[sana.arrayj.types.ArrayTypeApi]] is that
 * the type here has a reference to {{{java.lang.Object}}} type, while the one
 * in `arrayj` does not need it. And this one is a RefType, while the other one
 * doesn't.
 */
trait ArrayType extends RefType {
  /** A reference to the {{{java.lang.Object}}} type */
  def objectClassType: Type

  /** The type of the component of this array-type */
  def componentType: Type

  /** @see {{{RefType.parents}}} */
  def parents: Set[Symbol] = Set.empty

  /** @see {{{RefType.name}}} */
  def name: Name   = componentType match {
    case ct: RefType =>
      Name(s"[${ct.name.asString}]")
    case _           =>
      Name(s"[${componentType.toString}]")
  }


  def =:=(t: Type): Boolean = t match {
    case that: ArrayType    => this.componentType =:= that.componentType
    case _                  => false
  }


  def <:<(t: Type): Boolean = t match {
    case that: ArrayType    => this.componentType <:< that.componentType
    case _                  => t =:= objectClassType
  }

  override def toString: String = s"$componentType[]"
}

trait ArrayTypeExtractor {
  def unapply(tpe: ArrayType): Option[Type] = tpe match {
    case tpe: ArrayType      => Some(tpe.componentType)
    case null                => None
  }
}


object ArrayType extends ArrayTypeExtractor

class ArrayTypeImpl(val componentType: Type,
                      val objectClassType: Type) extends ArrayType
