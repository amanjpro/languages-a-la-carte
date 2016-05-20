package ch.usi.inf.l3.sana.guod.modifiers



import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj
import sana.robustj
import sana.ppj
import sana.modulej


import tiny.modifiers.{Flags, Flag}
import primj.modifiers._
import ooj.modifiers._
import robustj.modifiers._
import modulej.modifiers._
import ppj.modifiers._

import org.objectweb.asm.Opcodes

trait ModifiersUtils {
  def modifiersToBytecode(flags: Flags): Int = {
    modifiersToBytecodeAux(flags.flagsAsSet.toList)
  }
  protected def modifiersToBytecodeAux(flags: List[Flag]): Int = flags match {
    case (f::fs)                  =>
      f match {
        case INTERFACE            =>
          Opcodes.ACC_INTERFACE + modifiersToBytecodeAux(fs)
        case ABSTRACT             =>
          Opcodes.ACC_ABSTRACT + modifiersToBytecodeAux(fs)
        case PUBLIC_ACC           =>
          Opcodes.ACC_PUBLIC + modifiersToBytecodeAux(fs)
        case PRIVATE_ACC          =>
          Opcodes.ACC_PRIVATE + modifiersToBytecodeAux(fs)
        case PROTECTED_ACC        =>
          Opcodes.ACC_PROTECTED + modifiersToBytecodeAux(fs)
        case FINAL                =>
          Opcodes.ACC_FINAL + modifiersToBytecodeAux(fs)
        case NATIVE               =>
          Opcodes.ACC_NATIVE + modifiersToBytecodeAux(fs)
        case STATIC               =>
          Opcodes.ACC_STATIC + modifiersToBytecodeAux(fs)
        case SYNCHRONIZED         =>
          Opcodes.ACC_SYNCHRONIZED + modifiersToBytecodeAux(fs)
        case VOLATILE             =>
          Opcodes.ACC_VOLATILE + modifiersToBytecodeAux(fs)
        case TRANSIENT            =>
          Opcodes.ACC_TRANSIENT + modifiersToBytecodeAux(fs)
        case _                    =>
          modifiersToBytecodeAux(fs)
      }
    case _                        => Opcodes.NOP
  }
}


object ModifiersUtils extends ModifiersUtils

