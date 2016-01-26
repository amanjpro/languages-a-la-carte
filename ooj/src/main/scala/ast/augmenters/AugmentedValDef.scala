package ch.usi.inf.l3.sana.ooj.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.names.Name
import sana.primj.ast.ValDefApi



trait AugmentedValDef {

  def tree: ValDefApi

  def hasDefaultInit: Boolean =
    tree.attributes.get('hasDefaultInit)
      .map(_.asInstanceOf[Boolean]).getOrElse(false)

  def hasDefaultInit_=(flag: Boolean): Unit =
    tree.attributes = tree.attributes + ('hasDefaultInit -> flag)
}

