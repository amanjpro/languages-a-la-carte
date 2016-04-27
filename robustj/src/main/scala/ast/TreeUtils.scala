package ch.usi.inf.l3.sana.robustj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import sana.arrayj
import sana.arrooj
import sana.robustj


import tiny.ast._
import primj.ast.{MethodDefApi => _, _}
import ooj.ast.{MethodDefApi => _, _}
import brokenj.ast.BreakApi
import calcj.ast.Constant
import robustj.ast._
import robustj.ast.TreeExtractors._


trait TreeUtils extends arrooj.ast.TreeUtils {



  override def allPathsReturn(expr: Tree): Boolean =
    allPathsReturnAux(expr, allPathsReturn)

  override protected def allPathsReturnAux(expr: Tree,
        recurse: Tree => Boolean): Boolean = expr match {
    case tri: TryApi                   =>
      val tf      = recurse(tri.tryClause)
      lazy val cf =
        tri.catches.foldLeft(true)((z, y) => z && recurse(y))
      lazy val ff = tri.finallyClause.map(recurse(_)).getOrElse(true)
      tf && cf && ff
    case ctch: CatchApi                =>
      recurse(ctch.catchClause)
    case thrw: ThrowApi                =>
      true
    case _                             =>
      super.allPathsReturnAux(expr, recurse)
  }


  override def isValidStatement(e: Tree): Boolean = e match {
    case _: TryApi | _: ThrowApi    => true
    case _                          => super.isValidStatement(e)
  }


  override def isSimpleExpression(e: Tree): Boolean = e match {
    case _: TryApi                        => false
    case _: CatchApi                      => false
    case _: ThrowApi                      => false
    case _                                => super.isSimpleExpression(e)
  }



  override def canHaveLabel(tree: Tree): Boolean = tree match {
    case _: TryApi | _: ThrowApi        => true
    case _                              => super.canHaveLabel(tree)
  }
}

object TreeUtils extends TreeUtils
