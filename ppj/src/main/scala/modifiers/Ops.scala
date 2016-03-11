package ch.usi.inf.l3.sana.ppj.modifiers



import ch.usi.inf.l3.sana
import sana.tiny
import sana.robustj
import tiny.modifiers.Flags

object Ops {

  val noflags = tiny.modifiers.Ops.noflags


  implicit class FlagOps(mask: Flags)
      extends robustj.modifiers.Ops.FlagOps(mask) {
    val isSynchronized: Boolean            = mask.hasFlag(SYNCHRONIZED)
    val isVolatile: Boolean                = mask.hasFlag(VOLATILE)
  }
}
