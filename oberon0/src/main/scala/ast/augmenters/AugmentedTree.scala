package ch.usi.inf.l3.sana.oberon0.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.ast.Tree
import sana.tiny.names.Name



trait AugmentedTree extends sana.tiny.ast.augmenters.AugmentedTree {

  def endName: Option[Name] = {
    tree.attributes.get('endName).map(_.asInstanceOf[Name])
  }

  def endName_=(name: Name): Unit = {
    tree.attributes = tree.attributes + ('endName -> name)
  }


}
