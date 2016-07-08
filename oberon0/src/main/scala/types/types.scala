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

package ch.usi.inf.l3.sana.oberon0.types



import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.arrayj
import sana.brokenj
import sana.ooj
import sana.oberon0



import tiny.types.Type
import tiny.names.Name
import ooj.types.RefType
import tiny.ast.Expr

trait RecordTypeApi extends RefType {
  def fields: Map[Name, Type]


  def name: Name = Name("<Record type>")
  def <:<(other: Type): Boolean = other match {
    case that: RecordTypeApi         =>
      fields.foldLeft(true)((z, y) => {
        val (n, t) = y
        val r = that.fields.get(n).map(t <:< _).getOrElse(false)
        z && r
      })
    case _                           =>
      false
  }

  def =:=(other: Type): Boolean = other match {
    case that: RecordTypeApi         =>
      if(this.fields.size == that.fields.size) {
        fields.foldLeft(true)((z, y) => {
          val (n, t) = y
          val r = that.fields.get(n).map(t =:= _).getOrElse(false)
          z && r
        })
      } else false
    case _                           =>
      false
  }
}



trait ArrayTypeApi extends arrayj.types.ArrayTypeApi {
  def componentType: Type
  def size: Expr

  def name: Name = Name("<Array type>")
  override def =:=(other: Type): Boolean = this == other
  override def <:<(other: Type): Boolean = this =:= other
}


case class RecordType(val fields: Map[Name, Type]) extends RecordTypeApi
case class ArrayType(val componentType: Type,
  val size: Expr) extends ArrayTypeApi
