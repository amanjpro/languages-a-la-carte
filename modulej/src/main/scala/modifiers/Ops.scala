package ch.usi.inf.l3.sana.modulej.modifiers



import ch.usi.inf.l3.sana
import sana.tiny
import sana.ppj
import tiny.modifiers.Flags

object Ops {
  val noflags = tiny.modifiers.Ops.noflags


  implicit class FlagOps(mask: Flags)
      extends ppj.modifiers.Ops.FlagOps(mask) {
    val isNative:   Boolean            = mask.hasFlag(NATIVE)
  }
}
