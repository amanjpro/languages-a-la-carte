package ch.usi.inf.l3.sana.arrooj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.ooj
import sana.arrayj
import tiny.ast.{Tree, UseTree}
import ooj.ast.{TreeUtils => OTreeUtils}
import arrayj.ast.{TreeUtils => ATreeUtils}
import TreeExtractors._
import arrayj.ast._

trait TreeUtils extends OTreeUtils {

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

  def isArrayInitialization(e: Tree): Boolean =
    ATreeUtils.isArrayInitialization(e)


  def isArrayTypeUse(e: Tree): Boolean =
    ATreeUtils.isArrayTypeUse(e)

  def isArrayAccess(tree: Tree): Boolean = tree match {
    case ArrayAccess(_, _)            => true
    case _                            => false
  }

  def isArrayAccessOrVariableAccess(tree: Tree): Boolean = tree match {
    case ArrayAccess(array, _)        => isArrayAccessOrVariableAccess(array)
    case tree                         => isVariable(tree)
  }
}

object TreeUtils extends TreeUtils
