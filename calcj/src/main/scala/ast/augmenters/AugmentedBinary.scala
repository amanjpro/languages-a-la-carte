package ch.usi.inf.l3.sana.calcj.ast.augmenters

import ch.usi.inf.l3.sana
import sana.calcj.ast.BinaryApi



trait AugmentedBinary {

  def tree: BinaryApi

  def isCompoundBinary: Boolean =
    tree.attributes.get('isCompoundBinary)
      .map(_.asInstanceOf[Boolean])
      .getOrElse(false)

  def isCompoundBinary_=(flag: Boolean): Unit =
    tree.attributes = tree.attributes + ('isCompoundBinary -> flag)

  def isTypedCompoundBinary: Boolean =
    tree.attributes.get('isTypedCompoundBinary)
      .map(_.asInstanceOf[Boolean])
      .getOrElse(false)

  def isTypedCompoundBinary_=(flag: Boolean): Unit =
    tree.attributes = tree.attributes + ('isTypedCompoundBinary -> flag)



}

