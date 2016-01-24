package ch.usi.inf.l3.sana.primj.types

import ch.usi.inf.l3.sana
import sana.primj.names.StdNames
import sana.calcj.types._
import sana.calcj.typechecker.TypePromotions
import sana.tiny.types.Type
import sana.tiny.ast.{Tree, Expr}
import sana.tiny.ast.Implicits._
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


  def unifyTernaryBranches(lhs: Expr, rhs: Expr): Option[Type] = {
    (lhs.tpe, rhs.tpe) match {
      case (Some(t1), Some(t2)) if t1 =:= t2              =>
        Some(t1)
      case (Some(ByteType), Some(ShortType))              =>
        Some(ShortType)
      case (Some(ShortType), Some(ByteType))              =>
        Some(ShortType)
      case (Some(tpe1: NumericType),
            Some(tpe2: NumericType))                      =>
        if(TypePromotions.isNarrawableTo(rhs, tpe1)) {
          Some(tpe1)
        }
        else if(TypePromotions.isNarrawableTo(lhs, tpe2)) {
          Some(tpe2)
        } else {
          // INFO: This will be extended once we have OOJ
          Some(TypePromotions.binaryNumericPromotion(tpe1, tpe2))
        }
      case _                                              => None
    }
  }
}



object TypeUtils extends TypeUtils
