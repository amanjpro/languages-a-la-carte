package ch.usi.inf.l3.sana.calcj.names

import ch.usi.inf.l3.sana
import sana.tiny
import tiny.names.Name

trait StdNames extends tiny.names.StdNames {
  val BYTE_TYPE_NAME         = Name("byte")
  val SHORT_TYPE_NAME        = Name("short")
  val CHAR_TYPE_NAME         = Name("char")
  val INT_TYPE_NAME          = Name("int")
  val LONG_TYPE_NAME         = Name("long")

  val FLOAT_TYPE_NAME        = Name("float")
  val DOUBLE_TYPE_NAME       = Name("double")

  val BOOLEAN_TYPE_NAME      = Name("boolean")
}

object StdNames extends StdNames

