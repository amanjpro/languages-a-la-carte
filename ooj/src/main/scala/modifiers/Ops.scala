package ch.usi.inf.l3.sana.ooj.modifiers



import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.modifiers.Flags

object Ops {

  val noflags = tiny.modifiers.Ops.noflags


  implicit class FlagOps(mask: Flags)
      extends primj.modifiers.Ops.FlagOps(mask) {
    val isConstructor:   Boolean       = mask.hasFlag(CONSTRUCTOR)
    val isInterface: Boolean           = mask.hasFlag(INTERFACE)
    val isStatic: Boolean              = mask.hasFlag(STATIC)
    val isStaticInit: Boolean          = mask.hasFlag(STATIC_INIT)
    val isAbstract: Boolean            = mask.hasFlag(ABSTRACT)
    val isClass: Boolean               = mask.hasFlag(CLASS)

    val isPacakgeAcc: Boolean          = mask.hasFlag(PACKAGE_ACC)
    val isPrivateAcc: Boolean          = mask.hasFlag(PRIVATE_ACC)
    val isPublicAcc: Boolean           = mask.hasFlag(PUBLIC_ACC)
    val isProtectedAcc: Boolean        = mask.hasFlag(PROTECTED_ACC)

    val isOverride: Boolean            = mask.hasFlag(OVERRIDE)
  }
}
