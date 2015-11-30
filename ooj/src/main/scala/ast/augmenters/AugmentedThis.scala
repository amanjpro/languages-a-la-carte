package ch.usi.inf.l3.sana.ooj.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.symbols.Symbol
import sana.ooj.ast.ThisApi



trait AugmentedThis {

  def tree: ThisApi

  def enclosingClassSymbol: Option[Symbol] =
    tree.attributes.get('enclosingClassSymbol).map(_.asInstanceOf[Symbol])

  def enclosingClassSymbol_=(encl: Symbol): Unit =
    tree.attributes = tree.attributes + ('enclosingClassSymbol -> encl)
}

