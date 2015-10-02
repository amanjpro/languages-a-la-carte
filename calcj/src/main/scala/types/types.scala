package ch.usi.inf.l3.sana.calcj.types

import ch.usi.inf.l3.sana
import sana.tiny.types._


trait PrimitiveType extends Type {
  def =:=(other: Type): Boolean = this == other
}

trait NumericType extends PrimitiveType
trait IntegralType extends NumericType
trait DecimalType extends NumericType

case object IntType extends IntegralType {
  def <:<(other: Type): Boolean = other match {
    case IntType              => true
    case LongType             => true
    case _: DecimalType       => true
    case _                    => false
  }
}
case object ShortType extends IntegralType {
  def <:<(other: Type): Boolean = other match {
    case IntType              => true
    case LongType             => true
    case ShortType            => true
    case _: DecimalType       => true
    case _                    => false
  }
}
case object CharType extends IntegralType {
  def <:<(other: Type): Boolean = other match {
    case IntType              => true
    case LongType             => true
    case ShortType            => true
    case _: DecimalType       => true
    case _                    => false
  }
}
case object ByteType extends IntegralType {
  def <:<(other: Type): Boolean = other match {
    case IntType              => true
    case LongType             => true
    case ByteType             => true
    case _: DecimalType       => true
    case _                    => false
  }
}
case object LongType extends IntegralType {
  def <:<(other: Type): Boolean = other match {
    case LongType             => true
    case _: DecimalType       => true
    case _                    => false
  }
}


case object BooleanType extends PrimitiveType {
  def <:<(other: Type): Boolean = this =:= other
}


case object FloatType extends DecimalType {
  def <:<(other: Type): Boolean = other match {
    case _: DecimalType       => true
    case _                    => false
  }
}
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
