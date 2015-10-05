package ch.usi.inf.l3.sana.primj.typechecker


import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast._
import tiny.types._
import tiny.types.TypeUtils._
import tiny.symbols.{TypeSymbol, TermSymbol}
import tiny.source.Position
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.{TyperComponent, TypePromotions}
import calcj.types._
import calcj.ast.Unary
import calcj.ast.operators._
import primj.ast._
import primj.symbols._
import primj.errors.ErrorCodes._
import primj.types._
import primj.modifiers.Ops._



trait ProgramTyperComponent extends TyperComponent {

  def apply(tree: Tree): Tree = tree match {
    case program: Program          =>
      val newMembers = program.members.map(x => typed(x).asInstanceOf[DefTree])
      program.copy(members = newMembers)
  }

  def isDefinedAt(tree: Tree): Boolean   = defines(tree, "Program")
}



trait AssignTyperComponent extends TyperComponent {

  def apply(tree: Tree): Tree = tree match {
    case assign: Assign          =>
      val lhs = typed(assign.lhs)
      val rhs = typed(assign.rhs)
      (lhs, rhs) match {
        case (lhs: Tree, _) if ! TreeUtils.isVariable(lhs) =>
          error(ASSIGNING_NOT_TO_VARIABLE,
            lhs.toString, lhs.toString, lhs.pos, lhs)
          assign
        case (lhs: Tree, _)  if TreeUtils.isFinal(lhs)     =>
          error(REASSIGNING_FINAL_VARIABLE,
            lhs.toString, lhs.toString, lhs.pos, lhs)
          assign
        case (lhs: Expr, rhs: Expr)                        =>
          (lhs.tpe, rhs.tpe) match {
            case (Some(ltpe), Some(rtpe)) if ltpe >:> rtpe =>
              assign.copy(lhs = lhs, rhs = rhs)
            case (Some(ltpe), Some(rtpe))                  =>
              error(TYPE_MISMATCH,
                ltpe.toString, rtpe.toString, rhs.pos, assign)
              assign
            case _                                         =>
              error(TYPE_MISMATCH,
                lhs.toString, rhs.toString, rhs.pos, assign)
              assign
          }
        case _                                             =>
          // errors are already reported
          assign
      }
  }

  def isDefinedAt(tree: Tree): Boolean   = defines(tree, "Assign")
}


trait IfTyperComponent extends TyperComponent {
  def apply(tree: Tree): Tree = tree match {
    case ifelse: If           =>
      val cond  = typed(ifelse.cond)
      val thenp = typed(ifelse.thenp)
      val elsep = typed(ifelse.elsep)
      (cond, thenp, elsep) match {
        case (cond: Expr, thenp: Expr, elsep: Expr)     =>
          cond.tpe match {
            case Some(tpe) if tpe =:= BooleanType         =>
              ifelse.copy(cond = cond, thenp = thenp,
                elsep = elsep)
            case tpe                                      =>
              error(TYPE_MISMATCH,
                tpeToString(tpe),
                "boolean", ifelse.cond.pos, ifelse.cond)
              ifelse
          }
        case _                                          =>
          // errors are already reported
          ifelse
      }
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "If")
}


trait WhileTyperComponent extends TyperComponent {
  def apply(tree: Tree): Tree = tree match {
    case wile: While           =>
      val cond  = typed(wile.cond)
      val body  = typed(wile.body)
      (cond, body) match {
        case (cond: Expr, body: Expr)  =>
          cond.tpe match {
            case Some(tpe) if tpe =:= BooleanType         =>
              wile.copy(cond = cond, body = body)
            case tpe                                      =>
              error(TYPE_MISMATCH,
                tpeToString(tpe),
                "boolean", wile.cond.pos, wile.cond)
              wile
          }
        case _                =>
          // errors are already reported
          wile
      }
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "While")
}


trait BlockTyperComponent extends TyperComponent {
  def apply(tree: Tree): Tree = tree match {
    case block: Block           =>
      val stmts  = block.stmts.map(typed(_))
      block.copy(stmts = stmts)
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Block")
}


trait ForTyperComponent extends TyperComponent {
  def apply(tree: Tree): Tree = tree match {
    case forloop: For           =>
      val inits = forloop.inits.map(typed(_))
      val cond  = typed(forloop.cond)
      val steps = forloop.steps.map(typed(_).asInstanceOf[Expr])
      val body  = typed(forloop.body)
      (cond, body) match {
        case (cond: Expr, body: Expr)  =>
          cond.tpe match {
            case Some(tpe) if tpe =:= BooleanType         =>
              forloop.copy(inits = inits,
                           cond  = cond,
                           steps = steps,
                           body  = body)
            case tpe                                      =>
              error(TYPE_MISMATCH,
                tpeToString(tpe),
                "boolean", forloop.cond.pos, forloop.cond)
              forloop
          }
        case _                  =>
          // errors are already reported
          forloop
      }
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "For")
}


trait TernaryTyperComponent extends TyperComponent {
  def apply(tree: Tree): Tree = tree match {
    case ternary: Ternary                               =>
      val cond  = typed(ternary.cond)
      val thenp = typed(ternary.thenp)
      val elsep = typed(ternary.elsep)
      (cond, thenp, elsep) match {
        case (cond: Expr, thenp: Expr, elsep: Expr)     =>
          cond.tpe match {
            case Some(tpe) if tpe =:= BooleanType         =>
              val rtpe = unify(thenp, elsep)
              rtpe match {
                case None                     =>
                  error(TYPE_MISMATCH,
                    tpeToString(elsep.tpe),
                    tpeToString(thenp.tpe),
                    ternary.cond.pos, ternary.cond)
                  ternary
                case _                        =>
                  ternary.copy(cond = cond, thenp = thenp,
                    elsep = elsep, tpe = rtpe)
              }
            case tpe                                      =>
              error(TYPE_MISMATCH,
                tpeToString(tpe),
                "boolean", ternary.cond.pos, ternary.cond)
              ternary
          }
        case _                                          =>
          // errors are already reported
          ternary
      }
  }

  protected def unify(lhs: Expr, rhs: Expr): Option[Type] = {
    (lhs.tpe, rhs.tpe) match {
      case (Some(t1), Some(t2)) if t1 =:= t2              =>
        Some(t1)
      case (Some(ByteType), Some(ShortType))              =>
        Some(ShortType)
      case (Some(ShortType), Some(ByteType))              =>
        Some(ShortType)
      case (Some(tpe1: NumericType),
            Some(tpe2: NumericType))                      =>
        if((tpe1 =:= ShortType ||
            tpe1 =:= CharType  ||
            tpe1 =:= ByteType) &&
            tpe2 =:= IntType &&
            isConstantExpr(rhs) &&
            TypePromotions.isNarrawableTo(rhs, tpe1))
          Some(tpe1)
        else if((tpe2 =:= ShortType ||
                 tpe2 =:= CharType  ||
                 tpe2 =:= ByteType) &&
                 tpe1 =:= IntType &&
                 isConstantExpr(lhs) &&
                 TypePromotions.isNarrawableTo(lhs, tpe2))
          Some(tpe2)
        else
          // INFO: This will be extended once we have OOJ
          Some(TypePromotions.binaryNumericPromotion(tpe1, tpe2))
      case _                                              => None
    }
  }

  protected def isConstantExpr(tree: Tree): Boolean =
    TreeUtils.isConstantExpression(tree)

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Ternary")
}

trait ApplyTyperComponent extends TyperComponent {

  // FIXME: Apply doesn't work with method overloading
  def apply(tree: Tree): Tree = tree match {
    case apply: Apply   =>
      val fun    = typed(apply.fun).asInstanceOf[Expr]
      val funty  = fun.tpe
      val args   = apply.args.map(typed(_).asInstanceOf[Expr])
      val argtys = args.map(_.tpe).flatten
      (funty, argtys) match {
        case (Some(mt: MethodType), argtys) =>
          if(checkList(argtys, mt.params, _ <:< _)) {
            apply.copy(fun = fun, args = args)
          } else {
            // TODO: Fix the error message
            error(TYPE_MISMATCH, "", "", apply.pos, apply)
            apply
          }
        case _                                    =>
          error(BAD_STATEMENT,
            funty.toString, "function/method type", apply.pos, apply)
          apply
      }
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Apply")
}


trait ReturnTyperComponent extends TyperComponent {

  def apply(tree: Tree): Tree = tree match {
    case ret: Return          =>
      val expr  = ret.expr.map(typed(_).asInstanceOf[Expr]).getOrElse(NoTree)
      val tpe   = expr.tpe
      SymbolUtils.enclosingMethod(ret.owner) match {
        case Some(mthd) =>
          mthd.tpe match {
            case Some(MethodType(VoidType, _))  if expr == NoTree         =>
              ret
            case Some(MethodType(VoidType, _))                            =>
              error(NON_VOID_RETURN,
                ret.toString, ret.toString, ret.pos, ret)
              ret
            case Some(MethodType(_, _)) if expr == None                   =>
              error(VOID_RETURN,
                ret.toString, ret.toString, ret.pos, ret)
              ret
            case Some(MethodType(rtpe, _))                                =>
              expr.tpe.map(_ <:< rtpe) match {
                case Some(true)          =>
                  ret
                case _                   =>
                  error(TYPE_MISMATCH,
                    ret.toString, ret.toString, ret.pos, ret)
                  ret
              }
            case _                                                        =>
              error(TYPE_MISMATCH,
                tpe.toString, ret.toString, ret.pos, ret)
              ret
          }
        case _          =>
          // error should be reported by shape checker
          ret
      }
  }


  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Return")

}

trait UnaryTyperComponent extends calcj.typechecker.UnaryTyperComponent {
  override def apply(tree: Tree): Tree = tree match {
    case unary: Unary          =>
      super.apply(unary) match {
        case Unary(_, op, expr, _, _, _) if op == Inc || op == Dec    =>
          if(! TreeUtils.isVariable(expr))
            error(ASSIGNING_NOT_TO_VARIABLE,
              expr.toString, expr.toString, expr.pos, expr)
          else if(TreeUtils.isFinal(expr))
            error(REASSIGNING_FINAL_VARIABLE,
              expr.toString, expr.toString, expr.pos, expr)
          else ()
          unary
        case _                                                        =>
          unary
      }
  }

}

trait ValDefTyperComponent extends TyperComponent {
  def apply(tree: Tree): Tree = tree match {
    case valdef: ValDef          =>
      val tpt    = typed(valdef.tpt).asInstanceOf[UseTree]
      val rhs    = typed(valdef.rhs).asInstanceOf[Expr]
      val rtpe   = rhs.tpe.getOrElse(ErrorType)
      val ttpe   = tpt.tpe.getOrElse(ErrorType)
      if(ttpe =:= VoidType) {
        error(VOID_VARIABLE_TYPE,
            ttpe.toString, ttpe.toString, rhs.pos, valdef)
        valdef
      } else if(valdef.mods.isFinal && !valdef.mods.isParam &&
                rhs == NoTree) {
        error(UNINITIALIZED_FINAL_VARIABLE,
            valdef.toString, "", valdef.pos, valdef)
        valdef
      } else (rtpe <:< ttpe) match {
          case false if rhs != NoTree        =>
            error(TYPE_MISMATCH,
              rtpe.toString, ttpe.toString, rhs.pos, valdef)
            valdef
          case _                             =>
            val res = valdef.copy(tpt = tpt, rhs = rhs)
            res.symbol.foreach(_.tpe = Some(ttpe))
            res
        }
  }
  def isDefinedAt(tree: Tree): Boolean = defines(tree, "ValDef")
}


trait MethodDefTyperComponent extends TyperComponent {
  def apply(tree: Tree): Tree = tree match {
    case mthd: MethodDef          =>
      val tpt     = typed(mthd.ret).asInstanceOf[UseTree]
      val params  = mthd.params.map(typed(_).asInstanceOf[ValDef])
      val body    = typed(mthd.body).asInstanceOf[Expr]
      val tparams = params.map(_.tpe.getOrElse(ErrorType))
      val rtpe    = tpt.tpe.getOrElse(ErrorType)
      val mtpe    = MethodType(rtpe, tparams)
      val btpe    = body.tpe.getOrElse(ErrorType)
      if(btpe <:< rtpe && rtpe =/= VoidType) {
        error(TYPE_MISMATCH,
            rtpe.toString, btpe.toString, body.pos, mthd)
        mthd
      } else {
        // TODO: Check if all paths eventually return
        if(rtpe =/= VoidType && !allPathsReturn(body)) {
          error(MISSING_RETURN_STATEMENT,
            body.toString, body.toString, body.pos, mthd)
          mthd
        } else {
          val res = mthd.copy(ret = tpt,
            params = params, body = body)
          res.symbol.foreach(_.tpe = Some(mtpe))
          res
        }
      }
  }


  def allPathsReturn(expr: Tree): Boolean = TreeUtils.allPathsReturn(expr)
  def isDefinedAt(tree: Tree): Boolean = defines(tree, "MethodDef")
}


trait IdentTyperComponent extends TyperComponent {
  def apply(tree: Tree): Tree = tree match {
    case id: Ident     =>
      val symbol = id.symbol
      symbol match {
        case Some(sym: TermSymbol)  =>
          id
        case _                      =>
          error(NAME_NOT_FOUND,
            id.toString, "a term", id.pos, id)
          id
      }
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "Ident")
}


trait TypeUseTyperComponent extends TyperComponent {
  def apply(tree: Tree): Tree = tree match {
    case tuse: TypeUse     =>
      val symbol = tuse.symbol
      symbol match {
        case Some(sym: TypeSymbol)  =>
          tuse
        case Some(_)                =>
          error(TYPE_NAME_EXPECTED,
            tuse.toString, "a type", tuse.pos, tuse)
          tuse
        case _                      =>
          error(TYPE_NOT_FOUND,
            tuse.toString, "a type", tuse.pos, tuse)
          tuse
      }
  }

  def isDefinedAt(tree: Tree): Boolean = defines(tree, "TypeUse")
}
