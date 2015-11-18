package ch.usi.inf.l3.sana.primj.names

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.names.Name

trait StdNames extends calcj.names.StdNames {
  val VOID_NAME         = Name("void")
}

object StdNames extends StdNames

