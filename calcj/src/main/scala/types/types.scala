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

package ch.usi.inf.l3.sana.calcj.types

import ch.usi.inf.l3.sana
import sana.tiny.types._


trait PrimitiveType extends Type {
  def =:=(other: Type): Boolean = this == other
}

trait NumericType extends PrimitiveType
trait IntegralType extends NumericType
trait DecimalType extends NumericType

/** A type to represent the primitive {{{int}}}. */
case object IntType extends IntegralType {
  def <:<(other: Type): Boolean = other match {
    case IntType              => true
    case LongType             => true
    case _: DecimalType       => true
    case _                    => false
  }
}

/** A type to represent the primitive {{{short}}}. */
case object ShortType extends IntegralType {
  def <:<(other: Type): Boolean = other match {
    case IntType              => true
    case LongType             => true
    case ShortType            => true
    case _: DecimalType       => true
    case _                    => false
  }
}

/** A type to represent the primitive {{{char}}}. */
case object CharType extends IntegralType {
  def <:<(other: Type): Boolean = other match {
    case IntType              => true
    case LongType             => true
    case CharType             => true
    case _: DecimalType       => true
    case _                    => false
  }
}

/** A type to represent the primitive {{{byte}}}. */
case object ByteType extends IntegralType {
  def <:<(other: Type): Boolean = other match {
    case IntType              => true
    case LongType             => true
    case ByteType             => true
    case _: DecimalType       => true
    case _                    => false
  }
}

/** A type to represent the primitive {{{long}}}. */
case object LongType extends IntegralType {
  def <:<(other: Type): Boolean = other match {
    case LongType             => true
    case _: DecimalType       => true
    case _                    => false
  }
}


/** A type to represent the primitive {{{boolean}}}. */
case object BooleanType extends PrimitiveType {
  def <:<(other: Type): Boolean = this =:= other
}


/** A type to represent the primitive {{{float}}}. */
case object FloatType extends DecimalType {
  def <:<(other: Type): Boolean = other match {
    case _: DecimalType       => true
    case _                    => false
  }
}

/** A type to represent the primitive {{{double}}}. */
case object DoubleType extends DecimalType {
  def <:<(other: Type): Boolean = other match {
    case DoubleType           => true
    case _                    => false
  }
}


//
// case class UnaryType(operand: Type, res: Type) extends Type {
//   def =:=(other: Type): Boolean = {
//     lazy val temp = other match {
//       case UnaryType(o, r)   => o =:= operand && r =:= res
//       case _                 => false
//     }
//     other != null && (this == other || temp)
//   }
//
//   def <:<(other: Type): Boolean = {
//     lazy val temp = other match {
//       case UnaryType(o, r)   => o <:< operand && r <:< res
//       case _                 => false
//     }
//     other != null && (this =:= other || temp)
//   }
// }
//
// case class BinaryType(operand1: Type,
//   operand2: Type, res: Type) extends Type {
//
//   def =:=(other: Type): Boolean = {
//     lazy val temp = other match {
//       case BinaryType(o1, o2, r) =>
//         o1 =:= operand1 && o2 =:= operand2 && r =:= res
//       case _                     => false
//     }
//     other != null && (this == other || temp)
//   }
//
//   def <:<(other: Type): Boolean = {
//     lazy val temp = other match {
//       case BinaryType(o1, o2, r) =>
//         o1 <:< operand1 && o2 <:< operand2 && r <:< res
//       case _                     => false
//     }
//     other != null && (this =:= other || temp)
//   }
// }
