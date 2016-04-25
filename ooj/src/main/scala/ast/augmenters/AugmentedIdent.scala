package ch.usi.inf.l3.sana.ooj.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.symbols.Symbol
import sana.tiny.ast.IdentApi



trait AugmentedIdent {

  def tree: IdentApi

  def isMethodIdent: Boolean =
    tree.attributes.get('isMethod).map(_.asInstanceOf[Boolean]).getOrElse(false)

  def isMethodIdent_=(flag: Boolean): Unit =
    tree.attributes = tree.attributes + ('isMethod -> flag)


  def argumentTypes: Option[List[Type]] =
    tree.attributes.get('argumentTypes).map(_.asInstanceOf[List[Type]])

  def argumentTypes_=(argTypes: List[Type]): Unit =
    tree.attributes = tree.attributes + ('argumentTypes -> argTypes)

  def isConstructorIdent: Boolean =
    tree.attributes.get('isConstructorIdent)
      .map(_.asInstanceOf[Boolean]).getOrElse(false)

  def isConstructorIdent_=(flag: Boolean): Unit =
    tree.attributes = tree.attributes + ('isConstructorIdent -> flag)


  def isExplicitConstructorInvocation: Boolean =
    tree.attributes.get('isExplicitConstructorInvocation)
      .map(_.asInstanceOf[Boolean]).getOrElse(false)

  def isExplicitConstructorInvocation_=(flag: Boolean): Unit =
    tree.attributes =
      tree.attributes + ('isExplicitConstructorInvocation -> flag)

}

