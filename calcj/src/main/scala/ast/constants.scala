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

package ch.usi.inf.l3.sana.calcj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj

import tiny.ast._
import tiny.types._
import calcj.types._

/** The supertype of all constant values in this framework */
trait Constant {
  /** The type of which this constant value has */
  type Value <: Any

  /** The value of this constant */
  def value: Value


  /** The type-information of this constant value */
  def tpe: Type
}

object Constant {
  def unapply(const: Constant): Option[Any] = const match {
    case null                 => None
    case _                    => Some(const.value)
  }
}

/** A constant of type {{{byte}}} */
case class ByteConstant(value: Byte) extends Constant {
  type Value = Byte

  val tpe: Type = ByteType
}

/** A constant of type {{{char}}} */
case class CharConstant(value: Char) extends Constant {
  type Value = Char

  val tpe: Type = CharType
}

/** A constant of type {{{short}}} */
case class ShortConstant(value: Short) extends Constant {
  type Value = Short

  val tpe: Type = ShortType
}

/** A constant of type {{{int}}} */
case class IntConstant(value: Int) extends Constant {
  type Value = Int

  val tpe: Type = IntType
}

/** A constant of type {{{long}}} */
case class LongConstant(value: Long) extends Constant {
  type Value = Long

  val tpe: Type = LongType
}


/** A constant of type {{{float}}} */
case class FloatConstant(value: Float) extends Constant {
  type Value = Float

  val tpe: Type = FloatType
}

/** A constant of type {{{double}}} */
case class DoubleConstant(value: Double) extends Constant {
  type Value = Double

  val tpe: Type = DoubleType
}

/** A constant of type {{{boolean}}} */
case class BooleanConstant(value: Boolean) extends Constant {
  type Value = Boolean

  val tpe: Type = BooleanType
}
