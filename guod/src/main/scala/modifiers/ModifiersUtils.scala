/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
  /**
   * Translates Sana's modifiers to JVM's modifiers
   *
   * @param flags Sana's modifiers
   */
  def modifiersToBytecode(flags: Flags): Int = {
    modifiersToBytecodeAux(flags.flagsAsSet.toList)
  }

  /**
   * Translates Sana's modifiers to JVM's modifiers
   *
   * @param flags Sana's modifiers
   */
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
