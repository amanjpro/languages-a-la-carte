package ch.usi.inf.l3.sana.primj.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.ast.SimpleUseTree

trait AugmentedSimpleUseTree {

  def tree: SimpleUseTree

  def hasBeenNamed: Boolean =
    tree.attributes.get('hasBeenNamed)
      .map(_.asInstanceOf[Boolean])
      .getOrElse(false)

  def hasBeenNamed_=(flag: Boolean): Unit =
    tree.attributes = tree.attributes + ('hasBeenNamed -> flag)
}

