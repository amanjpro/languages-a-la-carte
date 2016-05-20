package ch.usi.inf.l3.sana.guod.names

import ch.usi.inf.l3.sana
import sana.robustj
import sana.tiny
import tiny.names.Name

trait StdNames extends robustj.names.StdNames {
  val CLINIT_NAME              = Name("<clinit>")
}

object StdNames extends StdNames

