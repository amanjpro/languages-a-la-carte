package ch.usi.inf.l3.sana.modulej.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.ast.UseTree



trait AugmentedUseTree {

  def tree: UseTree


  def isImportQual: Boolean =
    tree.attributes.get('isImportQual)
      .map(_.asInstanceOf[Boolean]).getOrElse(false)

  def isImportQual_=(flag: Boolean): Unit =
    tree.attributes = tree.attributes + ('isImportQual -> flag)

  def fullyQualifiedName: Option[String] =
    tree.attributes.get('fullyQualifiedName).map(_.asInstanceOf[String])

  def fullyQualifiedName_=(fname: String): Unit =
    tree.attributes = tree.attributes + ('fullyQualifiedName -> fname)

}


