package ch.usi.inf.l3.sana.primj.types

import ch.usi.inf.l3.sana
import sana.primj.names.StdNames
import sana.calcj.types._
import sana.tiny.types.Type
import sana.primj.symbols.SymbolUtils


trait TypeUtils extends sana.tiny.types.TypeUtils {


  /**
   * This method doesn't guarantee assignability if ltpe
   * is either short, byte or char and rpte is int.
   * Otherwise it does.
   * Further check (after constant-folding) is needed to
   * guarantee full assignability.
   * The method always returns true if ltpe is byte, char or
   * short and rtpe is int, otherwise it falls back to {{{<:<}}}
   */
  def isProbablyAssignable(ltpe: Type, rtpe: Type): Boolean = {
    (ltpe, rtpe) match {
      case (ShortType, IntType) |
           (CharType, IntType)  |
           (ByteType, IntType)           => true
      case _                             => rtpe <:< ltpe
    }
  }
}



object TypeUtils extends TypeUtils
