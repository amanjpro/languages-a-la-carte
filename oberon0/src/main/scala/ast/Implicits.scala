package ch.usi.inf.l3.sana.oberon0.ast

import ch.usi.inf.l3.sana.tiny.ast.{Tree, SimpleUseTree}
import ch.usi.inf.l3.sana.primj.ast.augmenters._
import ch.usi.inf.l3.sana.calcj.ast.BinaryApi
import ch.usi.inf.l3.sana.calcj.ast.augmenters._
import ch.usi.inf.l3.sana.arrayj.ast.augmenters._
import ch.usi.inf.l3.sana.arrayj.ast.ArrayInitializerApi
import augmenters._

object Implicits {
  implicit class AugmentedTreeImpl(val tree: Tree) extends AugmentedTree
  implicit class AugmentedBinaryImpl(val tree: BinaryApi) extends AugmentedBinary

  implicit class AugmentedSimpleUseTreeImpl(val tree: SimpleUseTree) extends
      AugmentedSimpleUseTree

  implicit class AugmentedArrayInitializerImpl(val tree: ArrayInitializerApi)
    extends AugmentedArrayInitializer
}
