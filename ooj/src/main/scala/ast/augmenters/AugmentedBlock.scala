package ch.usi.inf.l3.sana.ooj.ast.augmenters

import ch.usi.inf.l3.sana
import sana.primj.ast.BlockApi
import sana.tiny.symbols.Symbol



trait AugmentedBlock {

  def tree: BlockApi

  def isStaticInit: Boolean =
    tree
      .attributes
      .get('isStaticInit)
      .map(_.asInstanceOf[Boolean])
      .getOrElse(false)

  def isStaticInit_=(flag: Boolean): Unit =
    tree.attributes = tree.attributes + ('isStaticInit -> flag)
}

