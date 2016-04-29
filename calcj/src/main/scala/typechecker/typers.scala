package ch.usi.inf.l3.sana.calcj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj

import tiny.dsl._
import tiny.core.{TransformationComponent, TransformationFamily}
import tiny.ast.{TreeCopiers => _, TreeFactories => _, _}
import sana.calcj.ast.Implicits._
import sana.calcj.symbols.SymbolUtils
import tiny.types._
import tiny.symbols.Symbol
import tiny.errors.ErrorCodes._
import tiny.errors.ErrorReporting.{error,warning}
import calcj.ast._
import calcj.symbols._
import calcj.ast.operators._
import calcj.types._


trait TyperComponent extends
  TransformationComponent[Tree, Tree] {
  def typed: Tree => Tree
}


@component
trait BinaryTyperComponent extends TyperComponent {

  (bin: BinaryApi)           => {
    if(!bin.isTypedBinary) {
      val e1 = typed(bin.lhs)
      val e2 = typed(bin.rhs)
      (e1, e2) match {
        case (e1: Expr, e2: Expr)       if e1.tpe != None && e2.tpe != None =>
          val btpe = binaryTyper(e1.tpe.get, e2.tpe.get, bin)
          btpe match {
            case Some((e1tpe, e2tpe, rtpe))           =>
              val expr1 =
                compiler.typeCheck(bin.owner)(
                  castIfNeeded(e1, e1tpe, e1.tpe.get)).asInstanceOf[Expr]
              val expr2 =
                compiler.typeCheck(bin.owner)(
                  castIfNeeded(e2, e2tpe, e2.tpe.get)).asInstanceOf[Expr]
              val res = TreeCopiers.copyBinary(bin)(lhs = expr1, rhs = expr2)
              res.tpe = rtpe
              e1.tpe match {
                case Some(tpe)   if tpe.isInstanceOf[PrimitiveType] &&
                                    bin.isCompoundBinary                =>
                  SymbolUtils.getSymbol(tpe) match {
                    case Some(symbol)           =>
                      val tuse = TreeFactories.mkTypeUse(symbol.name,
                        expr1.pos, Some(symbol), bin.owner)
                      res.isCompoundBinary = false
                      res.isTypedCompoundBinary = true
                      res.isTypedBinary = true
                      compiler.typeCheck(bin.owner)(
                        TreeFactories.mkCast(tuse, res, pos = expr1.pos))
                    case _                      =>
                      res
                  }
                case _                                                  =>
                  res
              }
            case _                                    =>
              // errors are already reported
              bin
          }
        case _                                                              =>
          // errors are already reported
          bin
      }
    } else bin
  }

  protected def castIfNeeded(e: Expr, t1: Type, t2: Type): Expr = {
    TypePromotions.castIfNeeded(e, t1, t2)
  }


  protected def binaryTyper(ltpe: Type,
    rtpe: Type, bin: BinaryApi): Option[(Type, Type, Type)] = bin.op match {
      case Gt | Lt | Le | Ge                      =>
        (ltpe, rtpe) match {
          case (x: NumericType, y: NumericType)   =>
            val t = TypePromotions.binaryNumericPromotion(x, y)
            Some((t, t, BooleanType))
          case (_: NumericType, _)                =>
            error(TYPE_MISMATCH,
                rtpe.toString, "a numerical type", bin.rhs.pos)
            None
          case _                                  =>
            error(TYPE_MISMATCH,
                ltpe.toString, "a numerical type", bin.lhs.pos)
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
                ltpe.toString, "a primitive type", bin.pos)
            None
        }
      case And | Or                               =>
        (ltpe, rtpe) match {
          case (BooleanType, BooleanType)         =>
            Some((BooleanType, BooleanType, BooleanType))
          case (BooleanType, _)                   =>
            error(TYPE_MISMATCH,
              rtpe.toString, "boolean", bin.rhs.pos)
            None
          case _                                  =>
            error(TYPE_MISMATCH,
              ltpe.toString, "boolean", bin.lhs.pos)
            None
        }
      // case Add                                    =>
      //   (ltpe, rtpe) match {
      //     case (x: NumericType, y: NumericType)   =>
      //       val t = TypePromotions.binaryNumericPromotion(x, y)
      //       Some((t, t, t))
      //     case (_: NumericType, _)                =>
      //       error(TYPE_MISMATCH,
      //         rtpe.toString, "a numerical type", bin.rhs.pos)
      //       None
      //     case _                                  =>
      //       error(TYPE_MISMATCH,
      //         ltpe.toString, "a numerical type", bin.lhs.pos)
      //       None
      //   }
      case Sub | Mul | Div | Mod | Add            =>
        (ltpe, rtpe) match {
          case (BooleanType, BooleanType)         =>
            Some((BooleanType, BooleanType, BooleanType))
          case (x: NumericType, y: NumericType)   =>
            val t = TypePromotions.binaryNumericPromotion(x, y)
            Some((t, t, t))
          case (_: NumericType, _)                =>
            error(TYPE_MISMATCH,
              rtpe.toString, "a numerical type", bin.rhs.pos)
            None
          case _                                  =>
            error(TYPE_MISMATCH,
              ltpe.toString, "a numerical type", bin.lhs.pos)
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
                bin.pos)
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
              rtpe.toString, "an integral type", bin.rhs.pos)
            None
          case _                                  =>
            error(TYPE_MISMATCH,
              ltpe.toString, "an integral type", bin.lhs.pos)
            None
        }
      }

}

@component
trait UnaryTyperComponent extends TyperComponent {

  (unary: UnaryApi)          => {
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
            val res = TreeCopiers.copyUnary(unary)(expr = expr2)
            res.tpe = rtpe
            res
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
    unary: UnaryApi): Option[(Type, Type)] = {
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
        tpe.toString, "boolean", unary.expr.pos)
        None
      case (Pos, _) | (Neg, _) | (Inc, _) | (Dec, _)       =>
        error(TYPE_MISMATCH,
            tpe.toString, "a numeric type", unary.expr.pos)
        None
      case _                                               =>
        error(TYPE_MISMATCH,
            tpe.toString, "an integral type", unary.expr.pos)
        None
    }
  }

}

@component
trait CastTyperComponent extends TyperComponent {
  (cast: CastApi)           => {
    val tpt  = typed(cast.tpt)
    val expr = typed(cast.expr)
    (tpt, expr) match {
      case (tpt: UseTree, expr: Expr)   =>
        tpt.tpe.foreach(cast.tpe = _)
        tpt.symbol.foreach(cast.symbol = _)
        TreeCopiers.copyCast(cast)(tpt = tpt, expr = expr)
      case _                            =>
        // errors are already reported
        cast
    }
  }

}

@component
trait LiteralTyperComponent extends TyperComponent {
  (lit: LiteralApi)     => {
    lit.tpe    = lit.constant.tpe
    getSymbol(lit.constant.tpe).foreach {
      lit.symbol = _
    }
    lit
  }

  protected def getSymbol(t: Type): Option[Symbol] =
    SymbolUtils.getSymbol(t)
}
