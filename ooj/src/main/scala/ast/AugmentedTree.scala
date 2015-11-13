package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.symbols.Symbol
import sana.tiny.source.Position



trait AugmentedTree extends sana.tiny.ast.AugmentedTree {

  def enclosing: Option[Symbol] = {
    tree.attributes.get('enclosing).map(_.asInstanceOf[Symbol])
  }

  def enclosing_=(enclosing: Symbol): Unit = {
    tree.attributes = tree.attributes + ('enclosing -> enclosing)
  }

  def enclosingClassSymbol: Option[Symbol] = {
    tree.attributes.get('enclosingClassSymbol).map(_.asInstanceOf[Symbol])
  }

  def enclosingClassSymbol_=(encl: Symbol): Unit = {
    tree.attributes = tree.attributes + ('enclosingClassSymbol -> encl)
  }
}

