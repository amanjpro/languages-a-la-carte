package ch.usi.inf.l3.sana.calcj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny.ast._
import sana.tiny.ast.Implicits._
import sana.calcj.ast.TreeFactories._
import sana.tiny.types._
import sana.calcj.types._
import sana.calcj.ast._
import sana.calcj.symbols.SymbolUtils._


object TypePromotions {
  def binaryNumericPromotion(t1: NumericType,
    t2: NumericType): PrimitiveType = (t1, t2) match {
      case (DoubleType, _) => DoubleType
      case (_, DoubleType) => DoubleType
      case (FloatType, _)  => FloatType
      case (_, FloatType)  => FloatType
      case (LongType, _)   => LongType
      case (_, LongType)   => LongType
      case (_, _)          => IntType
    }

  def unaryNumericPromotion(t1: NumericType): NumericType = t1 match {
    case LongType        => LongType
    case x: IntegralType => IntType
    case _               => t1
  }



  def castIfNeeded(e: Expr, t1: Type, t2: Type): Expr = {
    if(t1 =:= t2) e
    else {
      val pos = e.pos
      getSymbol(t1) match {
        case Some(sym) =>
          val tuse = mkTypeUse(sym.name,
                            pos, Some(sym),
                            sym.owner)
          sym.tpe.foreach(tuse.tpe = _)
          mkCast(tuse, e)
        case _         =>
          e
      }
    }
  }


  def isNarrawableTo(expr: Expr, tpe: Type): Boolean = ???
}

