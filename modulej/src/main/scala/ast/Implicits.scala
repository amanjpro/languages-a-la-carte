package ch.usi.inf.l3.sana.modulej.ast

import ch.usi.inf.l3.sana.tiny.ast.augmenters._
import ch.usi.inf.l3.sana.arrayj.ast.augmenters._
import ch.usi.inf.l3.sana.ooj.ast.augmenters._
import ch.usi.inf.l3.sana.modulej.ast.augmenters._
import ch.usi.inf.l3.sana.tiny.ast.{Tree, SimpleUseTree, IdentApi, TypeUseApi,
                                    UseTree}
import ch.usi.inf.l3.sana.primj.ast.{BlockApi, ValDefApi}
import ch.usi.inf.l3.sana.arrayj.ast.ArrayInitializerApi
import ch.usi.inf.l3.sana.ooj.ast.{ClassDefApi, ThisApi,
                                   SuperApi, MethodDefApi}

object Implicits {
  implicit class AugmentedArrayInitializerImpl(val tree: ArrayInitializerApi)
    extends AugmentedArrayInitializer

  implicit class AugmentedTreeImpl(val tree: Tree) extends AugmentedTree
  implicit class AugmentedThisImpl(val tree: ThisApi) extends AugmentedThis
  implicit class AugmentedSuperImpl(val tree: SuperApi) extends AugmentedSuper
  implicit class AugmentedSimpleUseTreeImpl(val tree: SimpleUseTree)
      extends AugmentedSimpleUseTree
  implicit class AugmentedUseTreeImpl(val tree: UseTree)
      extends AugmentedUseTree

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



  implicit class AugmentedClassDefImpl(val tree: ClassDefApi)
      extends AugmentedClassDef

}
