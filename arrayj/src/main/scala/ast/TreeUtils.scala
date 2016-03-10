package ch.usi.inf.l3.sana.arrayj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.brokenj
import tiny.ast.{Tree, UseTree}
import TreeExtractors._

trait TreeUtils extends brokenj.ast.TreeUtils {

  override def isTypeUse(tree: UseTree): Boolean = tree match {
    case ArrayTypeUse(tpt)             =>
      isTypeUse(tpt)
    case _                             =>
      super.isTypeUse(tree)
  }

  override def isValidExpression(e: Tree): Boolean = e match {
    case _: ArrayCreationApi => true
    case _                   => super.isValidExpression(e)
  }

  def isArrayInitialization(e: Tree): Boolean = e match {
    case _: ArrayInitializerApi => true
    case _                      => false
  }


  def isArrayTypeUse(e: Tree): Boolean = e match {
    case _: ArrayTypeUseApi => true
    case _                  => false
  }


  def isArrayAccessOrVariableAccess(tree: Tree): Boolean = tree match {
    case ArrayAccess(array, _)        => isArrayAccessOrVariableAccess(array)
    case tree                         => isVariable(tree)
  }
}

object TreeUtils extends TreeUtils
