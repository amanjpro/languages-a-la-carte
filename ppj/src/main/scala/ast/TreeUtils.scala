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

trait TreeUtils extends robustj.ast.TreeUtils {
  override def isValidStatement(e: Tree): Boolean = e match {
    case _: SynchronizedApi => true
    case _                  => super.isValidStatement(e)
  }


  override def allPathsReturn(expr: Tree): Boolean = expr match {
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
