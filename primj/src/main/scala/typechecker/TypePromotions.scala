package ch.usi.inf.l3.sana.primj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny.ast.Expr
import sana.tiny.ast.Implicits._
import sana.primj.ast.TreeFactories._
import sana.tiny.types._
import sana.calcj.types._
import sana.calcj.ast._
import sana.primj.symbols.SymbolUtils


trait TypePromotions extends sana.calcj.typechecker.TypePromotions {
  def widenIfNeeded(expr: Expr, otpe: Option[Type]): Expr = {
    val res = for {
      etpe <- expr.tpe
      tpe  <- otpe      if etpe <:< tpe &&
                             etpe =/= tpe &&
                             etpe.isInstanceOf[PrimitiveType]
      sym  <- SymbolUtils.getSymbol(tpe)
    } yield {
      val tuse = TreeFactories.mkTypeUse(sym.name, expr.pos)
      val cast = TreeFactories.mkCast(tuse, expr, expr.pos)
      expr.owner.foreach { owner =>
        tuse.owner = owner
        cast.owner = owner
      }
      cast
    }
    res.getOrElse(expr)
  }
}

object TypePromotions extends TypePromotions
