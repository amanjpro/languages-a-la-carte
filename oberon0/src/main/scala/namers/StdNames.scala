package ch.usi.inf.l3.sana.oberon0.names

import ch.usi.inf.l3.sana
import sana.ooj
import sana.tiny

import tiny.names.Name


trait StdNames extends ooj.names.StdNames {
  val UNRESOLVED           = Name("<UNRESOLVED>")

  val NEW_TYPE_NAME_BASE   = Name("$NEW_TYPE_NAME_")
}



object StdNames extends StdNames

