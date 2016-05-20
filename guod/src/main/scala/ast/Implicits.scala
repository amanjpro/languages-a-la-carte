package ch.usi.inf.l3.sana.guod.ast

import ch.usi.inf.l3.sana.tiny.ast.augmenters._
import ch.usi.inf.l3.sana.primj.ast.augmenters.{AugmentedSimpleUseTree => _,
  _}
import ch.usi.inf.l3.sana.ooj.ast.augmenters.{AugmentedIdent => _,
            AugmentedValDef => _, AugmentedMethodDef => _, _}
import ch.usi.inf.l3.sana.calcj.ast.augmenters._
import ch.usi.inf.l3.sana.arrayj.ast.augmenters._
import ch.usi.inf.l3.sana.modulej.ast.augmenters._
import ch.usi.inf.l3.sana.guod.ast.augmenters._


object Implicits {
  implicit class AugmentedArrayInitializerImpl(val tree: ArrayInitializerApi)
    extends AugmentedArrayInitializer

  implicit class AugmentedBinaryImpl(val tree: BinaryApi) extends AugmentedBinary
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


  implicit class AugmentedSynchronizedImpl(val tree: SynchronizedApi)
    extends AugmentedSynchronized

  implicit class AugmentedTryImpl(val tree: TryApi)
    extends AugmentedTry

  implicit class AugmentedClassDefImpl(val tree: ClassDefApi)
      extends AugmentedClassDef

}
