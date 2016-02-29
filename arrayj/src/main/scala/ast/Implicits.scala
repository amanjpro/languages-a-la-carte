package ch.usi.inf.l3.sana.arrayj.ast

import ch.usi.inf.l3.sana.tiny.ast.augmenters._
import ch.usi.inf.l3.sana.tiny.ast.{Tree, SimpleUseTree}
import ch.usi.inf.l3.sana.primj.ast.augmenters._
import augmenters._

object Implicits {
  implicit class AugmentedTreeImpl(val tree: Tree) extends AugmentedTree

  implicit class AugmentedSimpleUseTreeImpl(val tree: SimpleUseTree) extends
      AugmentedSimpleUseTree

  implicit class AugmentedArrayInitializerImpl(val tree: ArrayInitializerApi)
    extends AugmentedArrayInitializer
}
