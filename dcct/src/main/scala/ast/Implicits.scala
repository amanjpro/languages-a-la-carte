package ch.usi.inf.l3.sana.dcct.ast

import ch.usi.inf.l3.sana.tiny.ast.augmenters._
import ch.usi.inf.l3.sana.primj.ast.augmenters._
import ch.usi.inf.l3.sana.tiny.ast.{Tree, TypeUseApi, SimpleUseTree}
import augmenters._

object Implicits {
  implicit class AugmentedTypeUseApiImpl(val tree: TypeUseApi) extends
      AugmentedTypeUseApi
  implicit class AugmentedTreeImpl(val tree: Tree) extends AugmentedTree
  implicit class AugmentedSimpleUseTreeImpl(val tree: SimpleUseTree) extends
      AugmentedSimpleUseTree
}
