package ch.usi.inf.l3.sana.calcj.names

import ch.usi.inf.l3.sana
import sana.tiny
import tiny.names.Name

trait StdNames extends tiny.names.StdNames {
  val BYTE_NAME         = Name("byte")
  val SHORT_NAME        = Name("short")
  val CHAR_NAME         = Name("char")
  val INT_NAME          = Name("int")
  val LONG_NAME         = Name("long")

  val FLOAT_NAME        = Name("float")
  val DOUBLE_NAME       = Name("double")

  val BOOLEAN_NAME      = Name("boolean")
}

object StdNames extends StdNames

