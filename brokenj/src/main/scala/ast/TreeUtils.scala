package ch.usi.inf.l3.sana.brokenj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import primj.ast
import tiny.ast.{Tree, NoTree}
import primj.ast._


trait TreeUtils extends ast.TreeUtils {

  override def isSimpleExpression(tree: Tree): Boolean = tree match {
    case _: ContinueApi                                   => false
    case _: BreakApi                                      => false
    case _: CaseApi                                       => false
    case _: SwitchApi                                     => false
    case _: LabelApi                                      => false
    case _                                             =>
      super.isSimpleExpression(tree)
  }

  override def allPathsReturn(tree: Tree): Boolean = tree match {
    // brokenj
    case _: ContinueApi | _: BreakApi                        =>
      false
    case label: LabelApi                                  =>
      allPathsReturn(label.stmt)
    case cse: CaseApi                                     =>
      allPathsReturn(cse.body)
    case switch: SwitchApi                                =>
      switch.cases.foldLeft(true)((z, y) =>
        z || allPathsReturn(y)
      )
    case e                                              =>
      super.allPathsReturn(e)
  }

  def canHaveLabel(tree: Tree): Boolean = tree match {
    // INFO: Synchronize, Throw and Try to be added
    case _: LabelApi | _: IfApi | _: WhileApi | _: ForApi | _: BlockApi |
            NoTree | _: SwitchApi | _: ContinueApi | _: BreakApi  |
         _: ReturnApi                                         =>
      true
    case e                                                 =>
      isValidStatementExpression(e)
  }

  def isContinuable(tree: Tree): Boolean = isLoopTree(tree)

  def isBreakable(tree: Tree): Boolean = tree match {
    case _: SwitchApi | _: WhileApi | _: ForApi    => true
    case _                                         => false
  }

  def isLoopTree(tree: Tree): Boolean = tree match {
    case _: WhileApi | _: ForApi            => true
    case _                                  => false
  }

}

object TreeUtils extends TreeUtils
