package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana.tiny.ast.{Tree, SimpleUseTree, IdentApi, TypeUseApi}
import ch.usi.inf.l3.sana.primj.ast.BlockApi
import ch.usi.inf.l3.sana.tiny.ast.augmenters._
import augmenters._

object Implicits {
  implicit class AugmentedTreeImpl(val tree: Tree) extends AugmentedTree
  implicit class AugmentedThisImpl(val tree: ThisApi) extends AugmentedThis
  implicit class AugmentedSuperImpl(val tree: SuperApi) extends AugmentedSuper
  implicit class AugmentedSimpleUseTreeImpl(val tree: SimpleUseTree)
      extends AugmentedSimpleUseTree

  implicit class AugmentedMethodDefImpl(val tree: MethodDefApi)
      extends AugmentedMethodDef

  implicit class AugmentedBlockImpl(val tree: BlockApi)
      extends AugmentedBlock
  implicit class AugmentedIdentImpl(val tree: IdentApi)
      extends AugmentedIdent
  implicit class AugmentedTypeUseImpl(val tree: TypeUseApi)
    extends AugmentedTypeUse
}

