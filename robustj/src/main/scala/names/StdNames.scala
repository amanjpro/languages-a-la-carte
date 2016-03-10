package ch.usi.inf.l3.sana.robustj.names

import ch.usi.inf.l3.sana
import sana.ooj
import sana.tiny
import tiny.names.Name

trait StdNames extends ooj.names.StdNames {
  val THROWABLE_CLASS_NAME              = Name("Throwable")
  val EXCEPTION_CLASS_NAME              = Name("Exception")
  val RUNTIME_EXCEPTION_CLASS_NAME      = Name("RuntimeException")
  val ERROR_CLASS_NAME                  = Name("Error")
}

object StdNames extends StdNames

