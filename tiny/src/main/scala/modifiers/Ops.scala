package ch.usi.inf.l3.sana.tiny.modifiers




object Ops {
  val noflags: Flags = NoFlags


  implicit class FlagOps(mask: Flags) {
    val isCompiled: Boolean  = mask.hasFlag(COMPILED)
  }
}
