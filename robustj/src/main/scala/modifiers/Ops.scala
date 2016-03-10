package ch.usi.inf.l3.sana.robustj.modifiers



import ch.usi.inf.l3.sana
import sana.tiny
import sana.ooj
import tiny.modifiers.Flags

object Ops {

  val noflags = tiny.modifiers.Ops.noflags


  implicit class FlagOps(mask: Flags)
      extends ooj.modifiers.Ops.FlagOps(mask) {
    val isExceptionParam: Boolean        = mask.hasFlag(EXCEPTION_PARAM)

    val isCatchSymbol: Boolean           = mask.hasFlag(CATCH_SYMBOL)
  }
}
