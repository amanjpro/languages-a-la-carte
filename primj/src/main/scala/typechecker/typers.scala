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


@component(tree, symbols)
trait ProgramTyperComponent extends TyperComponent {

  (program: ProgramApi)          => {
    val newMembers = program.members.map(x =>
        typed((x, symbols)).asInstanceOf[DefTree])
    TreeCopiers.copyProgram(program)(members = newMembers)
  }
}


@component(tree, symbols)
trait AssignTyperComponent extends TyperComponent {

  (assign: AssignApi)          => {
    val lhs = typed((assign.lhs, symbols))
    val rhs = typed((assign.rhs, symbols))
    (lhs, rhs) match {
      case (lhs: Tree, _) if ! TreeUtils.isVariable(lhs) =>
        error(ASSIGNING_NOT_TO_VARIABLE,
          lhs.toString, lhs.toString, lhs.pos)
        assign
      case (lhs: Tree, _)  if TreeUtils.isFinal(lhs)     =>
        error(REASSIGNING_FINAL_VARIABLE,
          lhs.toString, lhs.toString, lhs.pos)
        assign
      case (lhs: Expr, rhs: Expr)                        =>
        (lhs.tpe, rhs.tpe) match {
          case (Some(ltpe), Some(rtpe)) if ltpe >:> rtpe =>
            lhs.tpe.foreach(assign.tpe = _)
            TreeCopiers.copyAssign(assign)(lhs = lhs, rhs = rhs)
          case (Some(ltpe), Some(rtpe))                  =>
            error(TYPE_MISMATCH,
              ltpe.toString, rtpe.toString, rhs.pos)
            assign
          case _                                         =>
            error(TYPE_MISMATCH,
              lhs.toString, rhs.toString, rhs.pos)
            assign
        }
      case _                                             =>
        // errors are already reported
        assign
    }
  }
}


@component(tree, symbols)
trait IfTyperComponent extends TyperComponent {
  (ifelse: IfApi)           => {
    val cond  = typed((ifelse.cond, symbols))
    val thenp = typed((ifelse.thenp, symbols))
    val elsep = typed((ifelse.elsep, symbols))
    (cond, thenp, elsep) match {
      case (cond: Expr, thenp: Expr, elsep: Expr)     =>
        cond.tpe match {
          case Some(tpe) if tpe =:= BooleanType         =>
            TreeCopiers.copyIf(ifelse)(cond = cond, thenp = thenp,
              elsep = elsep)
          case tpe                                      =>
            error(TYPE_MISMATCH,
              tpeToString(tpe),
              "boolean", ifelse.cond.pos)
            ifelse
        }
      case _                                          =>
        // errors are already reported
        ifelse
    }
  }
}


@component(tree, symbols)
trait WhileTyperComponent extends TyperComponent {
  (wile: WhileApi)           => {
    val cond  = typed((wile.cond, symbols))
    val body  = typed((wile.body, symbols))
    (cond, body) match {
      case (cond: Expr, body: Expr)  =>
        cond.tpe match {
          case Some(tpe) if tpe =:= BooleanType         =>
            TreeCopiers.copyWhile(wile)(cond = cond, body = body)
          case tpe                                      =>
            error(TYPE_MISMATCH,
              tpeToString(tpe),
              "boolean", wile.cond.pos)
            wile
        }
      case _                =>
        // errors are already reported
        wile
    }
  }

}


@component(tree, symbols)
trait BlockTyperComponent extends TyperComponent {
  (block: BlockApi)           => {
    val stmts  = block.stmts.map(stmt => typed((stmt, symbols)))
    stmts match {
      case Nil    =>
        block.tpe = VoidType
      case _      =>
        stmts.last.tpe.foreach(block.tpe = _)
    }
    TreeCopiers.copyBlock(block)(stmts = stmts)
  }

}


@component(tree, symbols)
trait ForTyperComponent extends TyperComponent {
  (forloop: ForApi)           => {
    val inits = forloop.inits.map(init => typed((init, symbols)))
    val cond  = typed((forloop.cond, symbols))
    val steps = forloop.steps
      .map(step => typed((step, symbols)).asInstanceOf[Expr])
    val body  = typed((forloop.body, symbols))
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
              "boolean", forloop.cond.pos)
            forloop
        }
      case _                  =>
        // errors are already reported
        forloop
    }
  }

}


@component(tree, symbols)
trait TernaryTyperComponent extends TyperComponent {
  (ternary: TernaryApi)                               => {
    val cond  = typed((ternary.cond, symbols))
    val thenp = typed((ternary.thenp, symbols))
    val elsep = typed((ternary.elsep, symbols))
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
                  ternary.cond.pos)
                ternary
              case _                        =>
                rtpe.foreach(ternary.tpe = _)
                TreeCopiers.copyTernary(ternary)(cond = cond,
                  thenp = thenp, elsep = elsep)
            }
          case tpe                                      =>
            error(TYPE_MISMATCH,
              tpeToString(tpe),
              "boolean", ternary.cond.pos)
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

@component(tree, symbols)
trait ApplyTyperComponent extends TyperComponent {
  (apply: ApplyApi)   => {
    val fun    = typed((apply.fun, symbols)).asInstanceOf[Expr]
    val funty  = fun.tpe
    val args   = apply.args.map(arg => typed((arg, symbols)).asInstanceOf[Expr])
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
          error(TYPE_MISMATCH, "", "", apply.pos)
          apply
        }
      case _                                    =>
        error(BAD_STATEMENT,
          funty.toString, "function/method type", apply.pos)
        apply
    }
  }
}


@component(tree, symbols)
trait ReturnTyperComponent extends TyperComponent {
  (ret: ReturnApi)          => {
    val expr  = ret.expr
      .map(e => typed((e, symbols)).asInstanceOf[Expr]).getOrElse(NoTree)
    val tpe   = expr.tpe
    tpe.foreach(ret.tpe = _)
    SymbolUtils.enclosingMethod(ret.owner) match {
      case Some(mthd) =>
        mthd.tpe match {
          case Some(MethodType(VoidType, _))  if expr == NoTree         =>
            ret
          case Some(MethodType(VoidType, _))                            =>
            error(NON_VOID_RETURN,
              ret.tpe.map(_.toString).getOrElse(""),
              VoidType.toString, ret.pos)
            ret
          case Some(MethodType(t, _)) if expr == None                   =>
            error(VOID_RETURN,
              ret.tpe.map(_.toString).getOrElse(""),
              t.toString, ret.pos)
            ret
          case Some(MethodType(rtpe, _))                                =>
            expr.tpe.map(_ <:< rtpe) match {
              case Some(true)          =>
                ret
              case l                   =>
                error(TYPE_MISMATCH,
                  expr.tpe.map(_.toString).getOrElse("<error>"),
                  rtpe.toString, ret.pos)
                ret
            }
          case t                                                        =>
            error(TYPE_MISMATCH,
              tpe.toString, t.map(_.toString).getOrElse("A method type"),
              ret.pos)
            ret
        }
      case _          =>
        // error should be reported by shape checker
        ret
    }
  }


}

@component(tree, symbols)
trait UnaryTyperComponent extends calcj.typechecker.UnaryTyperComponent {
  (unary: UnaryApi) => {
    super.apply((unary, symbols)) match {
      case unary: UnaryApi if unary.op == Inc || unary.op == Dec    =>
        if(! TreeUtils.isVariable(unary.expr))
          error(ASSIGNING_NOT_TO_VARIABLE,
            unary.expr.toString, unary.expr.toString,
            unary.expr.pos)
        else if(TreeUtils.isFinal(unary.expr))
          error(REASSIGNING_FINAL_VARIABLE,
            unary.expr.toString, unary.expr.toString, unary.expr.pos)
        else ()
        unary
      case _                                                        =>
        unary
    }
  }
}


@component(tree, symbols)
trait ValDefTyperComponent extends TyperComponent {
  (valdef: ValDefApi)          => {
    val tpt    = typed((valdef.tpt, symbols)).asInstanceOf[UseTree]
    val rhs    = typed((valdef.rhs, symbols)).asInstanceOf[Expr]
    val rtpe   = rhs.tpe.getOrElse(ErrorType)
    val ttpe   = tpt.tpe.getOrElse(ErrorType)
    valdef.tpe = ttpe
    if(ttpe =:= VoidType) {
      error(VOID_VARIABLE_TYPE,
          ttpe.toString, ttpe.toString, rhs.pos)
      valdef
    } else if(valdef.mods.isFinal && !valdef.mods.isParam &&
              rhs == NoTree) {
      error(UNINITIALIZED_FINAL_VARIABLE,
          valdef.toString, "", valdef.pos)
      valdef
    } else (rtpe <:< ttpe) match {
        case false if rhs != NoTree        =>
          error(TYPE_MISMATCH,
            rtpe.toString, ttpe.toString, rhs.pos)
          valdef
        case _                             =>
          TreeCopiers.copyValDef(valdef)(tpt = tpt, rhs = rhs)
      }
  }
}


@component(tree, symbols)
trait MethodDefTyperComponent extends TyperComponent {
  (mthd: MethodDefApi)          => {
    val tpt     = typed((mthd.ret, symbols)).asInstanceOf[UseTree]
    val params  = mthd.params
      .map(param => typed((param, symbols)).asInstanceOf[ValDefApi])
    val tparams = params.map(_.tpe.getOrElse(ErrorType))
    val rtpe    = tpt.tpe.getOrElse(ErrorType)
    // if(!(btpe <:< rtpe) && rtpe =/= VoidType) {
    //   error(TYPE_MISMATCH,
    //       rtpe.toString, btpe.toString, body.pos, mthd)
    //   mthd
    // } else {
    // Check if all paths eventually return
    val tpe = MethodType(rtpe, tparams)
    mthd.tpe = tpe
    mthd.symbol.foreach( sym => {
      sym match {
        case m: MethodSymbol =>
          m.ret = tpt.symbol
        case _               =>
          ()
      }
      sym.tpe = Some(tpe)
    })
    // }
    val body    = typed((mthd.body, symbols)).asInstanceOf[Expr]
    val btpe    = body.tpe.getOrElse(ErrorType)

    if(rtpe =/= VoidType && !allPathsReturn(body)) {
      error(MISSING_RETURN_STATEMENT,
        body.toString, body.toString, body.pos)
      mthd
    } else {
      TreeCopiers.copyMethodDef(mthd)(ret = tpt,
        params = params, body = body)
    }
  }


  protected def allPathsReturn(expr: Tree): Boolean =
    TreeUtils.allPathsReturn(expr)
}


@component(tree, symbols)
trait IdentTyperComponent extends TyperComponent {
  (id: IdentApi)     => {
    val symbol = id.symbol
    symbol match {
      case Some(sym: TermSymbol)  =>
        sym.tpe.foreach(id.tpe = _)
        id
      case _                      =>
        error(NAME_NOT_FOUND,
          id.toString, "a term", id.pos)
        id
    }
  }

}


@component(tree, symbols)
trait TypeUseTyperComponent extends TyperComponent {
  (tuse: TypeUseApi)     => {
    val symbol = tuse.symbol
    symbol match {
      case Some(sym: TypeSymbol)  =>
        sym.tpe.foreach(tuse.tpe = _)
        tuse
      case Some(_)                =>
        error(TYPE_NAME_EXPECTED,
          tuse.toString, "a type", tuse.pos)
        tuse
      case _                      =>
        error(TYPE_NOT_FOUND,
          tuse.toString, "a type", tuse.pos)
        tuse
    }
  }

}
