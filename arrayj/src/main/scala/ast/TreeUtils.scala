package ch.usi.inf.l3.sana.arrayj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.brokenj
import tiny.ast.UseTree
import TreeExtractors._

trait TreeUtils extends brokenj.ast.TreeUtils {

  override def isTypeUse(tree: UseTree): Boolean = tree match {
    case ArrayTypeUse(tpt)             =>
      isTypeUse(tpt)
    case _                             =>
      super.isTypeUse(tree)
  }
}

object TreeUtils extends TreeUtils
