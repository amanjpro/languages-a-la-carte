package ch.usi.inf.l3.sana.ooj.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.symbols.Symbol
import sana.tiny.ast.TypeUseApi



trait AugmentedTypeUse {

  def tree: TypeUseApi

  def isInExtendsClause: Boolean =
    tree.attributes.get('isInExtendsClause)
      .map(_.asInstanceOf[Boolean]).getOrElse(false)

  def isInExtendsClause_=(flag: Boolean): Unit =
    tree.attributes = tree.attributes + ('isInExtendsClause -> flag)

  def isInImplementsClause: Boolean =
    tree.attributes.get('isInImplementsClause)
      .map(_.asInstanceOf[Boolean]).getOrElse(false)

  def isInImplementsClause_=(flag: Boolean): Unit =
    tree.attributes = tree.attributes + ('isInImplementsClause -> flag)


}

