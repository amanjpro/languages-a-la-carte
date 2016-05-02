package ch.usi.inf.l3.sana.oberon0.ast

import ch.usi.inf.l3.sana.tiny.ast.{Tree, SimpleUseTree, TypeUseApi, IdentApi}
import ch.usi.inf.l3.sana.calcj.ast.BinaryApi
import ch.usi.inf.l3.sana.calcj.ast.augmenters._
import ch.usi.inf.l3.sana.ooj.ast.augmenters._
import ch.usi.inf.l3.sana.primj.ast.{ValDefApi, MethodDefApi, BlockApi}
import ch.usi.inf.l3.sana.arrayj.ast.augmenters._
import ch.usi.inf.l3.sana.arrayj.ast.ArrayInitializerApi
import augmenters._

object Implicits {

  implicit class AugmentedArrayInitializerImpl(val tree: ArrayInitializerApi)
    extends AugmentedArrayInitializer

  implicit class AugmentedTreeImpl(val tree: Tree) extends AugmentedTree
  implicit class AugmentedSimpleUseTreeImpl(val tree: SimpleUseTree)
      extends AugmentedSimpleUseTree

  implicit class AugmentedValDefImpl(val tree: ValDefApi)
      extends AugmentedValDef

  implicit class AugmentedMethodDefImpl(val tree: MethodDefApi)
      extends AugmentedMethodDef
  implicit class AugmentedBlockImpl(val tree: BlockApi)
      extends AugmentedBlock
  implicit class AugmentedIdentImpl(val tree: IdentApi)
      extends AugmentedIdent
  implicit class AugmentedTypeUseImpl(val tree: TypeUseApi)
    extends AugmentedTypeUse
}