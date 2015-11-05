package ch.usi.inf.l3.sana.tiny.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.symbols.Symbol
import sana.tiny.source.Position



trait AugmentedTree {
  val tree: Tree
  def tpe: Option[Type] = {
    tree.attributes.get('type).map(_.asInstanceOf[Type])
  }

  def tpe_=(tpe: Type): Unit = {
    tree.attributes = tree.attributes + ('type -> tpe)
  }

  def owner: Option[Symbol] = {
    tree.attributes.get('owner).map(_.asInstanceOf[Symbol])
  }

  def owner_=(owner: Symbol): Unit = {
    tree.attributes = tree.attributes + ('owner -> owner)
  }

  def symbol: Option[Symbol] = {
    tree.attributes.get('symbol).map(_.asInstanceOf[Symbol])
  }

  def symbol_=(sym: Symbol): Unit = {
    tree.attributes = tree.attributes + ('symbol -> sym)
  }

  def pos: Option[Position] = {
    tree.attributes.get('position).map(_.asInstanceOf[Position])
  }

  def pos_=(pos: Position): Unit = {
    tree.attributes = tree.attributes + ('position -> pos)
  }
}


