package ch.usi.inf.l3.sana.guod.ast.augmenters

import ch.usi.inf.l3.sana
import sana.guod.ast.ValDefApi



trait AugmentedValDef extends sana.ooj.ast.augmenters.AugmentedValDef {

  def variableIndex: Option[Int] =
    tree.attributes.get('variableIndex).map(_.asInstanceOf[Int])

  def variableIndex_=(index: Int): Unit =
    tree.attributes = tree.attributes + ('variableIndex -> index)
}
