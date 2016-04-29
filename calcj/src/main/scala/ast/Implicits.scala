package ch.usi.inf.l3.sana.calcj.ast

import ch.usi.inf.l3.sana.tiny.ast.augmenters._
import ch.usi.inf.l3.sana.tiny.ast.{Tree, SimpleUseTree}
import augmenters._

object Implicits {
  implicit class AugmentedTreeImpl(val tree: Tree) extends AugmentedTree
  implicit class AugmentedBinaryImpl(val tree: BinaryApi) extends AugmentedBinary
}
