package ch.usi.inf.l3.sana.robustj.typechecker

import ch.usi.inf.l3.sana
import sana.robustj
import sana.arrooj
import sana.ooj
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj


import sana.dsl._
import calcj.typechecker.TyperComponent
import robustj.ast._
import tiny.ast.{Tree, NoTree, Expr, UseTree}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast.{ValDefApi, BlockApi}
import tiny.types.Type
import robustj.ast._
import robustj.errors.ErrorCodes._
import robustj.types.TypeUtils
import robustj.modifiers.Ops._
import arrooj.ast.Implicits._




@component
trait ThrowTyperComponent extends TyperComponent {
  (thrw: ThrowApi) => {
    val expr = typed(thrw.expr).asInstanceOf[Expr]
    checkThrownExpressionType(expr)
    TreeCopiers.copyThrow(thrw)(expr = expr)
  }
  protected def checkThrownExpressionType(expr: Expr): Unit = {
    expr.tpe.foreach { tpe =>
      if(!(tpe <:< throwableClassType))
        error(THROWING_NON_THROWABLE, "", "", expr.pos)
    }
  }

  protected def throwableClassType: Type =
    TypeUtils.throwableClassType
}


@component
trait TryTyperComponent extends TyperComponent {
  (tri: TryApi) => {
    val tryClause     = typed(tri.tryClause).asInstanceOf[BlockApi]
    val catches       =
      tri.catches.map(c => typed(c).asInstanceOf[CatchApi])
    val finallyClause =
      tri.finallyClause.map(fc => typed(fc).asInstanceOf[BlockApi])

    TreeCopiers.copyTry(tri)(tryClause = tryClause,
      catches = catches, finallyClause = finallyClause)
  }
}


@component
trait CatchTyperComponent extends TyperComponent {
  (ctch: CatchApi) => {
    val eparam        = typed(ctch.eparam).asInstanceOf[ValDefApi]
    checkCatchParamType(eparam)
    val catchClause   = typed(ctch.catchClause).asInstanceOf[BlockApi]

    TreeCopiers.copyCatch(ctch)(eparam = eparam,
      catchClause = catchClause)
  }

  protected def checkCatchParamType(param: ValDefApi): Unit = {
    param.tpe.foreach { tpe =>
      if(!(tpe <:< throwableClassType))
        error(CATCHING_NON_THROWABLE, "", "", param.pos)
    }
  }
  protected def throwableClassType: Type =
    TypeUtils.throwableClassType
}


@component
trait MethodDefTyperComponent
  extends ooj.typechecker.MethodDefTyperComponent {

  (mthd: MethodDefApi) => {
    val throwsClause =
      mthd.throwsClause.map(id => typed(id).asInstanceOf[UseTree])
    val res1         =
      super.apply(mthd).asInstanceOf[ooj.ast.MethodDefApi]
    // INFO: a bit of hack, but works
    val res2         = TreeFactories.mkMethodDef(res1.mods,
                                                  res1.ret,
                                                  res1.name,
                                                  res1.params,
                                                  throwsClause,
                                                  res1.body)
    res2.attributes = res1.attributes
    res2
  }

  override def allPathsReturn(expr: Tree): Boolean = {
    enclosingMethod(expr.symbol) match {
      case Some(mthd)                         =>
        mthd.mods.isAbstract || TreeUtils.allPathsReturn(expr)
      case None                               =>
        expr == NoTree
    }
  }

  protected def checkThrowsClause(throwsClause: List[UseTree]): Unit = for {
    id  <- throwsClause
    tpe <- id.tpe
  } {
    if(!(tpe <:< throwableClassType))
      error(NON_THROWABLE_IN_THROWS_CLAUSE, tpe.toString,
        throwableClassType.toString, id.pos)
  }

  protected def throwableClassType: Type =
    TypeUtils.throwableClassType
}
