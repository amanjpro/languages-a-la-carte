package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana.tiny.ast.{Tree, SimpleUseTree, IdentApi}
import augmenters._

object Implicits {
  implicit class AugmentedTreeImpl(val tree: Tree) extends AugmentedTree
  implicit class AugmentedSimpleUseTreeImpl(val tree: SimpleUseTree)
      extends AugmentedSimpleUseTree

  implicit class AugmentedIdentImpl(val tree: IdentApi)
      extends AugmentedIdent
}

