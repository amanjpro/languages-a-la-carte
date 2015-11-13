package ch.usi.inf.l3.sana.primj.typechecker


import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.ast.Implicits._
import tiny.types._
import tiny.types.TypeUtils._
import tiny.symbols.{TypeSymbol, TermSymbol}
import tiny.source.Position
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.{TyperComponent, TypePromotions}
import calcj.types._
import calcj.ast.UnaryApi
import calcj.ast.operators._
import primj.ast._
import primj.ast.TreeFactories._
import primj.symbols._
import primj.errors.ErrorCodes._
import primj.types._
import primj.modifiers.Ops._


@component
trait ProgramTyperComponent extends TyperComponent {

  (program: ProgramApi)          => {
    val newMembers = program.members.map(x => typed(x).asInstanceOf[DefTree])
    TreeCopiers.copyProgram(program)(members = newMembers)
  }
}


@component
trait AssignTyperComponent extends TyperComponent {

  (assign: AssignApi)          => {
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
            lhs.tpe.foreach(assign.tpe = _)
            TreeCopiers.copyAssign(assign)(lhs = lhs, rhs = rhs)
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
}


@component
trait IfTyperComponent extends TyperComponent {
  (ifelse: IfApi)           => {
    val cond  = typed(ifelse.cond)
    val thenp = typed(ifelse.thenp)
    val elsep = typed(ifelse.elsep)
    (cond, thenp, elsep) match {
      case (cond: Expr, thenp: Expr, elsep: Expr)     =>
        cond.tpe match {
          case Some(tpe) if tpe =:= BooleanType         =>
            TreeCopiers.copyIf(ifelse)(cond = cond, thenp = thenp,
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
}


@component
trait WhileTyperComponent extends TyperComponent {
  (wile: WhileApi)           => {
    val cond  = typed(wile.cond)
    val body  = typed(wile.body)
    (cond, body) match {
      case (cond: Expr, body: Expr)  =>
        cond.tpe match {
          case Some(tpe) if tpe =:= BooleanType         =>
            TreeCopiers.copyWhile(wile)(cond = cond, body = body)
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

}


@component
trait BlockTyperComponent extends TyperComponent {
  (block: BlockApi)           => {
    val stmts  = block.stmts.map(typed(_))
    stmts match {
      case Nil    =>
        block.tpe = VoidType
      case _      =>
        stmts.last.tpe.foreach(block.tpe = _)
    }
    TreeCopiers.copyBlock(block)(stmts = stmts)
  }

}


@component
trait ForTyperComponent extends TyperComponent {
  (forloop: ForApi)           => {
    val inits = forloop.inits.map(typed(_))
    val cond  = typed(forloop.cond)
    val steps = forloop.steps.map(typed(_).asInstanceOf[Expr])
    val body  = typed(forloop.body)
    (cond, body) match {
      case (cond: Expr, body: Expr)  =>
        cond.tpe match {
          case Some(tpe) if tpe =:= BooleanType         =>
            TreeCopiers.copyFor(forloop)(inits = inits,
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

}


@component
trait TernaryTyperComponent extends TyperComponent {
  (ternary: TernaryApi)                               => {
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
                rtpe.foreach(ternary.tpe = _)
                TreeCopiers.copyTernary(ternary)(cond = cond,
                  thenp = thenp, elsep = elsep)
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

}

@component
trait ApplyTyperComponent extends TyperComponent {
  // FIXME: Apply doesn't work with method overloading
  (apply: ApplyApi)   => {
    val fun    = typed(apply.fun).asInstanceOf[Expr]
    val funty  = fun.tpe
    val args   = apply.args.map(typed(_).asInstanceOf[Expr])
    val argtys = args.map(_.tpe).flatten
    (funty, argtys) match {
      case (Some(mt: MethodType), argtys) =>
        if(checkList(argtys, mt.params, _ <:< _)) {
          funty match {
            case Some(MethodType(r, _)) =>
              apply.tpe = r
            case _                      =>
              ()
          }
          TreeCopiers.copyApply(apply)(fun = fun, args = args)
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
}


@component
trait ReturnTyperComponent extends TyperComponent {
  (ret: ReturnApi)          => {
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


}

@component
trait UnaryTyperComponent extends calcj.typechecker.UnaryTyperComponent {
  (unary: UnaryApi) => {
    super.apply(unary) match {
      case unary: UnaryApi if unary.op == Inc || unary.op == Dec    =>
        if(! TreeUtils.isVariable(unary.expr))
          error(ASSIGNING_NOT_TO_VARIABLE,
            unary.expr.toString, unary.expr.toString,
            unary.expr.pos, unary.expr)
        else if(TreeUtils.isFinal(unary.expr))
          error(REASSIGNING_FINAL_VARIABLE,
            unary.expr.toString, unary.expr.toString, unary.expr.pos,
            unary.expr)
        else ()
        unary
      case _                                                        =>
        unary
    }
  }
}


@component
trait ValDefTyperComponent extends TyperComponent {
  (valdef: ValDefApi)          => {
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
          TreeCopiers.copyValDef(valdef)(tpt = tpt, rhs = rhs)
      }
  }
}


@component
trait MethodDefTyperComponent extends TyperComponent {
  (mthd: MethodDefApi)          => {
    val tpt     = typed(mthd.ret).asInstanceOf[UseTree]
    val params  = mthd.params.map(typed(_).asInstanceOf[ValDefApi])
    val body    = typed(mthd.body).asInstanceOf[Expr]
    val tparams = params.map(_.tpe.getOrElse(ErrorType))
    val rtpe    = tpt.tpe.getOrElse(ErrorType)
    val btpe    = body.tpe.getOrElse(ErrorType)
    if(!(btpe <:< rtpe) && rtpe =/= VoidType) {
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
        TreeCopiers.copyMethodDef(mthd)(ret = tpt,
          params = params, body = body)
      }
    }
  }


  def allPathsReturn(expr: Tree): Boolean = TreeUtils.allPathsReturn(expr)
}


@component
trait IdentTyperComponent extends TyperComponent {
  (id: IdentApi)     => {
    val symbol = id.symbol
    symbol match {
      case Some(sym: TermSymbol)  =>
        sym.tpe.foreach(id.tpe = _)
        id
      case _                      =>
        error(NAME_NOT_FOUND,
          id.toString, "a term", id.pos, id)
        id
    }
  }

}


@component
trait TypeUseTyperComponent extends TyperComponent {
  (tuse: TypeUseApi)     => {
    val symbol = tuse.symbol
    symbol match {
      case Some(sym: TypeSymbol)  =>
        sym.tpe.foreach(tuse.tpe = _)
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

}
