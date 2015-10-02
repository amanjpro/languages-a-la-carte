package ch.usi.inf.l3.sana.calcj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj

import tiny.ast._
import tiny.types._
import calcj.types._

trait Constant {
  type Value <: Any

  def value: Value

  def tpe: Type
}
object Constant {
  def unapply(const: Constant): Option[Any] = const match {
    case null                 => None
    case _                    => Some(const.value)
  }
}

case class ByteConstant(value: Byte) extends Constant {
  type Value = Byte

  val tpe: Type = ByteType
}

case class CharConstant(value: Char) extends Constant {
  type Value = Char

  val tpe: Type = CharType
}

case class ShortConstant(value: Short) extends Constant {
  type Value = Short

  val tpe: Type = ShortType
}

case class IntConstant(value: Int) extends Constant {
  type Value = Int

  val tpe: Type = IntType
}

case class LongConstant(value: Long) extends Constant {
  type Value = Long

  val tpe: Type = LongType
}


case class FloatConstant(value: Float) extends Constant {
  type Value = Float

  val tpe: Type = FloatType
}

case class DoubleConstant(value: Double) extends Constant {
  type Value = Double

  val tpe: Type = DoubleType
}

case class BooleanConstant(value: Boolean) extends Constant {
  type Value = Boolean

  val tpe: Type = BooleanType
}



