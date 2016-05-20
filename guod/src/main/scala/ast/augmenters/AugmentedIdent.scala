package ch.usi.inf.l3.sana.guod.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.symbols.Symbol
import sana.tiny.ast.IdentApi



trait AugmentedIdent extends sana.ooj.ast.augmenters.AugmentedIdent {

  def identifierIndex: Option[Int] =
    tree.attributes.get('identifierIndex).map(_.asInstanceOf[Int])

  def identifierIndex_=(index: Int): Unit =
    tree.attributes = tree.attributes + ('identifierIndex -> index)
}
