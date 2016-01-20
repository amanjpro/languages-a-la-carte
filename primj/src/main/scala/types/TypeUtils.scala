package ch.usi.inf.l3.sana.primj.types

import ch.usi.inf.l3.sana
import sana.primj.names.StdNames
import sana.calcj.types._
import sana.calcj.typechecker.TypePromotions
import sana.tiny.types.Type
import sana.tiny.ast.Tree
import sana.primj.symbols.SymbolUtils


trait TypeUtils extends sana.tiny.types.TypeUtils {


  def isAssignable(rtree: Tree, rtpe: Type, ltpe: Type): Boolean = {
    (ltpe, rtpe) match {
      case (ShortType, IntType) |
           (CharType, IntType)  |
           (ByteType, IntType)           =>
        TypePromotions.isNarrawableTo(rtree, ltpe)
      case _                             => rtpe <:< ltpe
    }
  }
}



object TypeUtils extends TypeUtils
