package ch.usi.inf.l3.sana.tiny.ast

import ch.usi.inf.l3.sana
// import sana.core.SyntaxComponent
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names._



trait Tree {
  def tpe: Option[Type]
  def pos: Option[Position]
  def owner: Option[Symbol]
}


trait SymTree extends Tree {
  def symbol: Option[Symbol]
  // def owner: Option[Symbol] = symbol.flatMap(_.owner)
}

trait NamedTree extends SymTree {
  def name: Name
}

trait DefTree extends NamedTree {
}

trait TermTree extends DefTree

trait TypeTree extends DefTree

trait Expr extends Tree

trait UseTree extends Expr with SymTree {
  val symbol: Option[Symbol]
}

trait SimpleUseTree extends UseTree


trait TypeUseApi extends SimpleUseTree
trait IdentApi extends SimpleUseTree

class TypeUse protected[ast](val name: Name, val pos: Option[Position],
  val symbol: Option[Symbol],
  val owner: Option[Symbol]) extends TypeUseApi {
  def tpe: Option[Type] = symbol.flatMap(_.tpe)

  override def toString: String = s"TypeUse(${name.asString})"

  override def equals(other: Any): Boolean = other match {
    case null                    => false
    case that: TypeUse           =>
      this.pos == that.pos &&
        this.symbol == that.symbol &&
        this.owner == that.owner
        this.name == that.name
    case _                       => false
  }

  override def hashCode(): Int =
    this.pos.hashCode +
      this.symbol.hashCode * 43 +
      this.owner.hashCode +
      this.name.hashCode * 13
}
object TypeUse {
  def unapply(tuse: TypeUse): Option[Symbol] = tuse match {
    case null               => None
    case _                  =>
      tuse.symbol
  }

  def apply(name: Name, pos: Option[Position]): TypeUse =
    new TypeUse(name, pos, None, None)

  def apply(symbol: Symbol, pos: Option[Position]): TypeUse =
    new TypeUse(symbol.name, pos, Some(symbol), symbol.owner)

  def apply(symbol: Option[Symbol], pos: Option[Position],
    owner: Option[Symbol]): TypeUse =
      new TypeUse(symbol.map(_.name).getOrElse(noname),
        pos, symbol, owner)


  def apply(name: Name, pos: Option[Position],
    owner: Option[Symbol]): TypeUse =
      new TypeUse(name, pos, None, owner)
}

class Ident protected[ast](val name: Name, val pos: Option[Position],
  val symbol: Option[Symbol],
  val owner: Option[Symbol]) extends IdentApi {

  def tpe: Option[Type] = symbol.flatMap(_.tpe)

  override def toString: String = s"Ident(${name.asString})"
  override def equals(other: Any): Boolean = other match {
    case null                    => false
    case that: Ident             =>
      this.pos == that.pos &&
        this.symbol == that.symbol &&
        this.owner == that.owner
        this.name == that.name
    case _                       => false
  }

  override def hashCode(): Int =
    this.pos.hashCode +
      this.symbol.hashCode * 43 +
      this.owner.hashCode +
      this.name.hashCode * 13

}
object Ident {
  def unapply(id: Ident): Option[Symbol] = id match {
    case null               => None
    case _                  =>
      id.symbol
  }

  def apply(name: Name, pos: Option[Position]): Ident =
    new Ident(name, pos, None, None)

  def apply(symbol: Symbol, pos: Option[Position]): Ident =
    new Ident(symbol.name, pos, Some(symbol), symbol.owner)

  def apply(symbol: Option[Symbol], pos: Option[Position],
    owner: Option[Symbol]): Ident =
      new Ident(symbol.map(_.name).getOrElse(noname), pos, symbol, owner)

  def apply(name: Name, pos: Option[Position],
    owner: Option[Symbol]): Ident =
      new Ident(name, pos, None, owner)

}

case object NoTree extends Expr {
  def tpe   = None
  def pos   = None
  def owner = None
}

case object ErrorTree extends Tree {
  def tpe   = None
  def pos   = None
  def owner = None
}
