package ch.usi.inf.l3.sana.ooj.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.symbols.Symbol
import sana.ooj.ast.ClassDefApi



trait AugmentedClassDef {

  def tree: ClassDefApi


  def sourceName: Option[String] =
    tree.attributes.get('sourceName)
      .map(_.asInstanceOf[String])

  def sourceName_=(name: String): Unit =
    tree.attributes = tree.attributes + ('sourceName -> name)


}


