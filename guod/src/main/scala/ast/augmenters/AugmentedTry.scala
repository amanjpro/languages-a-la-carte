package ch.usi.inf.l3.sana.guod.ast.augmenters

import ch.usi.inf.l3.sana
import sana.guod.ast.TryApi



trait AugmentedTry {

  def tree: TryApi

  def finallyParamIndex: Option[Int] =
    tree.attributes.get('finallyParamIndex).map(_.asInstanceOf[Int])

  def finallyParamIndex_=(index: Int): Unit =
    tree.attributes = tree.attributes + ('finallyParamIndex -> index)
}
