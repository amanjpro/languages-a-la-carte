package ch.usi.inf.l3.sana.ppj.typechecker

import ch.usi.inf.l3.sana
import sana.ppj
import sana.dynj
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
import tiny.ast.{Tree, Expr, NoTree}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast.BlockApi
import tiny.types.Type
import ppj.ast._
import ppj.errors.ErrorCodes._
import robustj.types.TypeUtils
import ppj.modifiers.Ops._
import arrooj.ast.Implicits._


@component
trait MethodDefTyperComponent
  extends robustj.typechecker.MethodDefTyperComponent {

  override def allPathsReturn(expr: Tree): Boolean = {
    enclosingMethod(expr.symbol) match {
      case Some(mthd)                         =>
        mthd.mods.isAbstract || TreeUtils.allPathsReturn(expr)
      case None                               =>
        expr == NoTree
    }
  }
}



@component
trait SynchronizedTyperComponent extends TyperComponent {
  (sync: SynchronizedApi) => {
    val expr = typed(sync.expr).asInstanceOf[Expr]
    val block = typed(sync.block).asInstanceOf[BlockApi]

    expr.tpe match {
      case Some(tpe)    if tpe <:< objectClassType => ()
      case _                                       =>
        error(REFERENCE_TYPE_EXPECTED, "", "", expr.pos)
    }

    TreeCopiers.copySynchronized(sync)(expr, block)
  }


  protected def objectClassType: Type =
    TypeUtils.objectClassType
}
