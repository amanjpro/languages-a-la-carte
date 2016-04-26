package ch.usi.inf.l3.sana.ppj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import sana.arrayj
import sana.arrooj
import sana.robustj
import sana.dynj
import sana.ppj


import tiny.ast.Tree
import primj.ast.{MethodDefApi => _, _}
import ooj.ast.{MethodDefApi => _, _}
import brokenj.ast.BreakApi
import calcj.ast.Constant
import robustj.ast._
import ppj.ast.TreeExtractors._


trait TreeUtils extends robustj.ast.TreeUtils {
  override def isValidStatement(e: Tree): Boolean = e match {
    case _: SynchronizedApi => true
    case _                  => super.isValidStatement(e)
  }


  override def allPathsReturn(expr: Tree): Boolean = expr match {
    case wile: WhileApi                      =>
      wile.cond match {
        case Literal(Constant(true))         =>
          wile.body match {
            case b: BlockApi                 =>
              !b.stmts.exists {
                case s: BreakApi             =>
                  true
                case s   if isBreakable(s)   =>
                  !allPathsReturn(s)
                case _                       =>
                  false
              }
            case _                           =>
              false
          }
        case _                               =>
          allPathsReturn(wile.body)
      }
    case forloop: ForApi                     =>
      forloop.cond match {
        case Literal(Constant(true))         =>
          forloop.body match {
            case b: BlockApi                 =>
              !b.stmts.exists {
                case s: BreakApi             =>
                  true
                case s   if isBreakable(s)   =>
                  !allPathsReturn(s)
                case _                       =>
                  false
              }
            case _                           =>
              false
          }
        case _                               =>
          allPathsReturn(forloop.body)
      }
    case tri: TryApi                   =>
      val tf      = allPathsReturn(tri.tryClause)
      lazy val cf =
        tri.catches.foldLeft(true)((z, y) => z && allPathsReturn(y))
      lazy val ff = tri.finallyClause.map(allPathsReturn(_)).getOrElse(true)
      tf && cf && ff
    case ctch: CatchApi                =>
      allPathsReturn(ctch.catchClause)
    case s: SynchronizedApi => allPathsReturn(s.block)
    case _                  => super.allPathsReturn(expr)
  }

  override def isSimpleExpression(tree: Tree): Boolean = tree match {
    case _: SynchronizedApi => false
    case _                  => super.isSimpleExpression(tree)
  }

  override def canHaveLabel(tree: Tree): Boolean = tree match {
    case _: SynchronizedApi => true
    case _                  => super.isValidStatement(tree)
  }
}

object TreeUtils extends TreeUtils
