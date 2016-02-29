package ch.usi.inf.l3.sana.arrayj.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.arrayj.ast.ArrayInitializerApi



trait AugmentedArrayInitializer {

  def tree: ArrayInitializerApi

  def componentType: Option[Type] =
    tree
      .attributes
      .get('componentType)
      .map(_.asInstanceOf[Type])

  def componentType_=(tpe: Type): Unit =
    tree.attributes = tree.attributes + ('componentType -> componentType)
}

