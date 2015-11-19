package ch.usi.inf.l3.sana.ooj.names

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import tiny.names.Name

trait StdNames extends primj.names.StdNames {
  val DEFAULT_PACKAGE_NAME   = Name("<default>")
  val CONSTRUCTOR_NAME       = Name("<init>")
  val JAVA_PACKAGE_NAME      = Name("java")
  val LANG_PACKAGE_NAME      = Name("lang")
  val OBJECT_TYPE_NAME       = Name("Object")
  val NULL_NAME              = Name("null")
}

object StdNames extends StdNames

