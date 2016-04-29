package ch.usi.inf.l3.sana.calcj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny.ast._
import sana.tiny.ast.Implicits._
import sana.calcj.ast.TreeFactories._
import sana.tiny.types._
import sana.calcj.types._
import sana.calcj.ast._
import sana.calcj.ast.TreeExtractors._
import sana.calcj.symbols.SymbolUtils._


trait TypePromotions {
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
                            e.owner)
          sym.tpe.foreach(tuse.tpe = _)
          mkCast(tuse, e, pos = pos)
        case _         =>
          e
      }
    }
  }


  def isNarrawableTo(expr: Tree, tpe: Type): Boolean = expr match {
    case Literal(c) if c.tpe =:= IntType =>
      val value = c.value.asInstanceOf[Int]
      if(tpe =:= ShortType) {
        value.isValidShort
      } else if(tpe =:= ByteType) {
        value.isValidByte
      } else if(tpe =:= CharType) {
        value.isValidChar
      } else false
    case _                               =>
      false
  }
}


object TypePromotions extends TypePromotions
