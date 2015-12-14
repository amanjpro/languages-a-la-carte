package ch.usi.inf.l3.sana.ooj.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.names.Name
import sana.primj.ast.MethodDefApi



trait AugmentedMethodDef {

  def tree: MethodDefApi

  def declaredClassNameForConstructor: Option[Name] =
    tree.attributes.get('declaredClassNameForConstructor)
      .map(_.asInstanceOf[Name])

  def declaredClassNameForConstructor_=(name: Name): Unit =
    tree.attributes = tree.attributes + ('declaredClassNameForConstructor -> name)
}

