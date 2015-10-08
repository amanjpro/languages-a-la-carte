package ch.usi.inf.l3.sana.calcj.typechecker

import ch.usi.inf.l3.sana
import sana.core.{TransformationComponent, TransformationFamily}
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast._
import tiny.types._
import tiny.errors.ErrorCodes._
import tiny.errors.ErrorReporting.{error,warning}
import calcj.ast._
import calcj.ast.operators._
import calcj.types._


trait TyperComponent extends TransformationComponent[Tree, Tree] {
  def typed: Tree => Tree
}


@component
trait BinaryTyperComponent extends TyperComponent {

  (bin: Binary)           => {
    val e1 = typed(bin.lhs)
    val e2 = typed(bin.rhs)
    (e1, e2) match {
      case (e1: Expr, e2: Expr)       if e1.tpe != None && e2.tpe != None =>
        val btpe = binaryTyper(e1.tpe.get, e2.tpe.get, bin)
        btpe match {
          case Some((e1tpe, e2tpe, rtpe))           =>
            val expr1 = TypePromotions.castIfNeeded(e1,
              e1tpe, e1.tpe.get)
            val expr2 = TypePromotions.castIfNeeded(e2,
              e2tpe, e2.tpe.get)
            bin.copy(lhs = expr1, rhs = expr2, tpe = Some(rtpe))
          case _                                    =>
            // errors are already reported
            bin
        }
      case _                                                              =>
        // errors are already reported
        bin
    }
  }


  def binaryTyper(ltpe: Type,
    rtpe: Type, bin: Binary): Option[(Type, Type, Type)] = bin.op match {
      case Gt | Lt | Le | Ge                      =>
        (ltpe, rtpe) match {
          case (x: NumericType, y: NumericType)   =>
            val t = TypePromotions.binaryNumericPromotion(x, y)
            Some((t, t, BooleanType))
          case (_: NumericType, _)                =>
            error(TYPE_MISMATCH,
                rtpe.toString, "a numerical type", bin.rhs.pos, bin.rhs)
            None
          case _                                  =>
            error(TYPE_MISMATCH,
                ltpe.toString, "a numerical type", bin.lhs.pos, bin.lhs)
            None
        }
      case Eq | Neq                               =>
        (ltpe, rtpe) match {
          case (BooleanType, BooleanType)         =>
            Some((BooleanType, BooleanType, BooleanType))
          case (x: NumericType, y: NumericType)   =>
            val t = TypePromotions.binaryNumericPromotion(x, y)
            Some((t, t, BooleanType))
          case _                                  =>
            error(TYPE_MISMATCH,
                ltpe.toString, "a primitive type", bin.pos, bin)
            None
        }
      case And | Or | Amp | Pipe | Xor            =>
        (ltpe, rtpe) match {
          case (BooleanType, BooleanType)         =>
            Some((BooleanType, BooleanType, BooleanType))
          case (BooleanType, _)                   =>
            error(TYPE_MISMATCH,
              rtpe.toString, "bolean", bin.rhs.pos, bin.rhs)
            None
          case _                                  =>
            error(TYPE_MISMATCH,
              ltpe.toString, "bolean", bin.lhs.pos, bin.lhs)
            None
        }
      case Add                                    =>
        (ltpe, rtpe) match {
          case (x: NumericType, y: NumericType)   =>
            val t = TypePromotions.binaryNumericPromotion(x, y)
            Some((t, t, t))
          case (_: NumericType, _)                =>
            error(TYPE_MISMATCH,
              rtpe.toString, "a numerical type", bin.rhs.pos, bin.rhs)
            None
          case _                                  =>
            error(TYPE_MISMATCH,
              ltpe.toString, "a numerical type", bin.lhs.pos, bin.lhs)
            None
        }
      case Sub | Mul | Div | Mod                  =>
        (ltpe, rtpe) match {
          case (x: NumericType, y: NumericType)   =>
            val t = TypePromotions.binaryNumericPromotion(x, y)
            Some((t, t, t))
          case (_: NumericType, _)                =>
            error(TYPE_MISMATCH,
              rtpe.toString, "a numerical type", bin.rhs.pos, bin.rhs)
            None
          case _                                  =>
            error(TYPE_MISMATCH,
              ltpe.toString, "a numerical type", bin.lhs.pos, bin.lhs)
            None
        }

      case BAnd | BOr | BXor                      =>
        (ltpe, rtpe) match {
          case (x: IntegralType, y: IntegralType) =>
            val t = TypePromotions.binaryNumericPromotion(x, y)
            Some((t, t, t))
          case _                                  =>
            error(TYPE_MISMATCH,
              bin.toString, "both operands should be integral types",
                bin.pos, bin)
            None
        }
      case SHL | SHR | USHR                       =>
        (ltpe, rtpe) match {
          case (x: IntegralType, y: IntegralType) =>
            val t1 = TypePromotions.unaryNumericPromotion(x)
            val t2 = TypePromotions.unaryNumericPromotion(y)
            Some((t1, t2, t1))
          case (_: IntegralType, _)               =>
            error(TYPE_MISMATCH,
              rtpe.toString, "an integral type", bin.rhs.pos, bin.rhs)
            None
          case _                                  =>
            error(TYPE_MISMATCH,
              ltpe.toString, "an integral type", bin.lhs.pos, bin.lhs)
            None
        }
      }

}

@component
trait UnaryTyperComponent extends TyperComponent {

  (unary: Unary)          => {
    // TODO:
    // Pos unary operator, should ideally perform the cast and return
    // the containing expression not the whole unary expression (the
    // operation is redundant). But this will reproduce the same problem
    // that Scala has, when type checker can return a different tree
    // type. What should we do here?
    // res        <- unary.op match {
    //   case Pos    => point(expr)
    //   case _      => point(Unary(unary.op, expr, point(utpe), unary.pos))
    // }
    typed(unary.expr) match {
      case expr: Expr       if expr.tpe != None =>
        val utpe = unaryTyper(expr.tpe.get, unary)
        utpe match {
          case Some((etpe, rtpe))            =>
            val expr2 = TypePromotions.castIfNeeded(expr,
              etpe, expr.tpe.get)
            unary.copy(expr = expr2, tpe = Some(rtpe))
          case _                             =>
            // errors are already reported
            unary
        }
      case _                                    =>
        // errors are already reported
        unary
    }
  }

  protected def unaryTyper(tpe: Type,
    unary: Unary): Option[(Type, Type)] = {
    (unary.op, tpe)  match {
      case (Not, BooleanType)                              =>
        Some((BooleanType, BooleanType))
      case (Pos, x: NumericType)                           =>
        val t = TypePromotions.unaryNumericPromotion(x)
        Some((x, t))
      case (Neg, x: NumericType)                           =>
        val t = TypePromotions.unaryNumericPromotion(x)
        Some((x, t))
      case (BCompl, x: IntegralType)                       =>
        val t = TypePromotions.unaryNumericPromotion(x)
        Some((x, t))
      case (Inc, x: NumericType)                           =>
        Some((x, x))
      case (Dec, x: NumericType)                           =>
        Some((x, x))
      case (Not, _)                                        =>
        error(TYPE_MISMATCH,
        tpe.toString, "boolean", unary.expr.pos, unary.expr)
        None
      case (Pos, _) | (Neg, _) | (Inc, _) | (Dec, _)       =>
        error(TYPE_MISMATCH,
            tpe.toString, "a numeric type", unary.expr.pos, unary.expr)
        None
      case _                                               =>
        error(TYPE_MISMATCH,
            tpe.toString, "an integral type", unary.expr.pos, unary.expr)
        None
    }
  }

}

@component
trait CastTyperComponent extends TyperComponent {

  (cast: Cast)           => {
    val tpt  = typed(cast.tpt)
    val expr = typed(cast.expr)
    (tpt, expr) match {
      case (tpt: UseTree, expr: Expr)   =>
        cast.copy(tpt = tpt, expr = expr)
      case _                            =>
        // errors are already reported
        cast
    }
  }

}

@component
trait LiteralTyperComponent extends TyperComponent {
  (lit: Literal)     => lit
}
