package ch.usi.inf.l3.sana.ooj.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.ast.SimpleUseTree
import sana.tiny.symbols.Symbol



trait AugmentedSimpleUseTree {

  def tree: SimpleUseTree

  def enclosing: Option[Symbol] =
    tree.attributes.get('enclosing).map(_.asInstanceOf[Symbol])

  def enclosing_=(enclosing: Symbol): Unit =
    tree.attributes = tree.attributes + ('enclosing -> enclosing)

  def shouldBeStatic: Boolean =
    tree.attributes.get('shouldBeStatic)
      .map(_.asInstanceOf[Boolean]).getOrElse(false)

  def shouldBeStatic_=(shouldBeStatic: Boolean): Unit =
    tree.attributes = tree.attributes + ('shouldBeStatic -> shouldBeStatic)



}

