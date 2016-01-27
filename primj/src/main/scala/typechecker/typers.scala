package ch.usi.inf.l3.sana.primj.typechecker


import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.names.Name
import primj.ast.Implicits._
import tiny.types.{TypeUtils => _, _}
import tiny.symbols.{Symbol, TypeSymbol, TermSymbol}
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
    val newMembers = program.members.map(x =>
        typed(x).asInstanceOf[DefTree])
    TreeCopiers.copyProgram(program)(members = newMembers)
  }
}


@component
trait AssignTyperComponent extends TyperComponent {

  (assign: AssignApi)          => {
    val lhs = typed(assign.lhs).asInstanceOf[Expr]
    val rhs = typed(assign.rhs).asInstanceOf[Expr]
    checkVariableLHS(lhs)
    checkFinalReassigning(lhs)
    checkAssignmentTypeCorrectness(lhs, rhs, assign)
  }


  protected def checkVariableLHS(lhs: Tree): Unit = {
    if(!TreeUtils.isVariable(lhs))
      error(ASSIGNING_NOT_TO_VARIABLE,
        lhs.toString, lhs.toString, lhs.pos)
  }

  protected def checkFinalReassigning(lhs: Tree): Unit = {
    if(TreeUtils.isFinal(lhs))
      error(REASSIGNING_FINAL_VARIABLE,
        lhs.toString, lhs.toString, lhs.pos)
  }

  protected def checkAssignmentTypeCorrectness(lhs: Expr, rhs: Expr,
    assign: AssignApi): AssignApi = {
    (lhs.tpe, rhs.tpe) match {
      case (Some(ltpe), Some(rtpe))
          if TypeUtils.isAssignable(rhs, rtpe, ltpe)  =>
        rhs.tpe.foreach(assign.tpe = _)
        TreeCopiers.copyAssign(assign)(lhs = lhs, rhs = rhs)
      case (Some(ltpe), Some(rtpe))                      =>
        error(TYPE_MISMATCH,
          ltpe.toString, rtpe.toString, rhs.pos)
        assign
      case _                                             =>
        error(TYPE_MISMATCH,
          lhs.toString, rhs.toString, rhs.pos)
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
              "boolean", ifelse.cond.pos)
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
              "boolean", wile.cond.pos)
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
    val stmts = block.stmts.map { stmt =>
      typed(stmt)
    }
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
    val inits = forloop.inits.map { init =>
      typed(init)
    }
    val cond  = typed(forloop.cond)
    val steps = forloop.steps
      .map(step => typed(step).asInstanceOf[Expr])
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
              "boolean", forloop.cond.pos)
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
            val rtpe = unifyTernaryBranches(thenp, elsep)
            rtpe match {
              case None                     =>
                error(TYPE_MISMATCH,
                  "",
                  "",
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

  protected def unifyTernaryBranches(lhs: Expr, rhs: Expr): Option[Type] =
    TypeUtils.unifyTernaryBranches(lhs, rhs)

  protected def isNarrawableTo(e: Tree, t: Type): Boolean =
    TypePromotions.isNarrawableTo(e, t)

  protected def binaryNumericPromotion(t1: NumericType,
    t2: NumericType): PrimitiveType =
    TypePromotions.binaryNumericPromotion(t1, t2)
}

@component
trait ApplyTyperComponent extends TyperComponent {
  (apply: ApplyApi)   => {
    val fun    = typed(apply.fun).asInstanceOf[Expr]
    val funty  = fun.tpe
    val args   = apply.args.map(arg => typed(arg).asInstanceOf[Expr])
    val argtys = args.map(_.tpe).flatten
    (funty, argtys) match {
      case (Some(mt: MethodType), argtys) =>
        if(TypeUtils.checkList(argtys, mt.params, _ <:< _)) {
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


@component
trait ReturnTyperComponent extends TyperComponent {
  (ret: ReturnApi)          => {
    val expr  = ret.expr
      .map(e => typed(e).asInstanceOf[Expr]).getOrElse(NoTree)
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
            val ok = expr.tpe.map(etpe =>
                TypeUtils.isAssignable(expr, etpe, rtpe))
            ok match {
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

@component
trait UnaryTyperComponent extends calcj.typechecker.UnaryTyperComponent {
  (unary: UnaryApi) => {
    super.apply(unary) match {
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
      case unary                                                    =>
        unary
    }
  }
}


@component
trait ValDefTyperComponent extends TyperComponent {
  (valdef: ValDefApi)          => {
    if(!valdef.mods.isField) {
      checkDoubleDef(valdef.owner, valdef.name, valdef.pos)
      valdef.owner.foreach(sym => {
        valdef.symbol.foreach(sym.declare(_))
      })
    }
    val tpt    = typed(valdef.tpt).asInstanceOf[UseTree]
    valdef.symbol.foreach(sym => {
      sym.tpe.foreach(valdef.tpe = _)
      sym match {
        case vs: VariableSymbol =>
          vs.typeSymbol = tpt.symbol
        case _                  =>
          ()
      }
    })
    val rhs    = typed(valdef.rhs).asInstanceOf[Expr]
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
    } else (TypeUtils.isAssignable(rhs, rtpe, ttpe)) match {
        case false if rhs != NoTree        =>
          error(TYPE_MISMATCH,
            rtpe.toString, ttpe.toString, rhs.pos)
          valdef
        case _                             =>
          TreeCopiers.copyValDef(valdef)(tpt = tpt, rhs = rhs)
      }
  }


  protected def checkDoubleDef(owner: Option[Symbol],
      name: Name, pos: Option[Position]): Unit =
    if(SymbolUtils.alreadyDefinedLocalVarable(owner, name))
      error(VARIABLE_ALREADY_DEFINED,
          "", "", pos)

}


@component
trait MethodDefTyperComponent extends TyperComponent {
  (mthd: MethodDefApi)          => {
    val tpt     = typed(mthd.ret).asInstanceOf[UseTree]
    val params  = mthd.params
      .map(param => typed(param).asInstanceOf[ValDefApi])
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
    val body    = typed(mthd.body).asInstanceOf[Expr]
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


@component
trait IdentTyperComponent extends TyperComponent {
  (id: IdentApi)     => {
    if(!id.hasBeenNamed) {
      val symbol = id.symbol match {
        case None           =>
          val sym = id.owner.flatMap(_.getSymbol(id.name,
              _.isInstanceOf[TermSymbol]))
          sym.foreach(id.symbol = _)
          sym
        case Some(sym)      =>
          Some(sym)
      }
      symbol match {
        case Some(sym: TermSymbol)  =>
          sym.tpe.foreach(id.tpe = _)
          id
        case _                      =>
          error(NAME_NOT_FOUND,
            id.toString, "a term", id.pos)
          id
      }
    } else id
  }

}


@component
trait TypeUseTyperComponent extends TyperComponent {
  (tuse: TypeUseApi)     => {
    if(!tuse.hasBeenNamed) {
      val symbol = tuse.symbol match {
        case None           =>
          val sym = tuse.owner.flatMap(_.getSymbol(tuse.name,
              _.isInstanceOf[TypeSymbol]))
          sym.foreach(tuse.symbol = _)
          sym
        case Some(sym)      =>
          Some(sym)
      }
      symbol match {
        case Some(sym: TypeSymbol)                =>
          sym.tpe.foreach(tuse.tpe = _)
          tuse
        case Some(_)                              =>
          error(TYPE_NAME_EXPECTED,
            tuse.toString, "a type", tuse.pos)
          tuse
        case _                                    =>
          error(TYPE_NOT_FOUND,
            tuse.toString, "a type", tuse.pos)
          tuse
      }
    } else tuse
  }

}
