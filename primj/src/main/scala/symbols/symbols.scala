package ch.usi.inf.l3.sana.primj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.primj.types.VoidType
import sana.tiny.symbols.{Symbol, TermSymbol, TypeSymbol}
import sana.tiny.modifiers.Flags
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.names.Name
import sana.tiny.names.StdNames.noname





object ProgramSymbol extends Symbol {
  decls = decls ++ SymbolUtils.standardDefinitions

  def name: Name = noname
  def mods: Flags = noflags
  def tpe: Option[Type] = None
  def owner: Option[Symbol] = None

  def name_=(name: Name): Unit = ???
  def mods_=(mods: Flags): Unit = ???
  def tpe_=(tpe: Option[Type]): Unit = ???
  def owner_=(owner: Option[Symbol]): Unit = ???
}

case class VariableSymbol(var mods: Flags, var name: Name,
  var typeSymbol: Option[Symbol], var owner: Option[Symbol])
  extends TermSymbol {

  def tpe: Option[Type] = typeSymbol.flatMap(_.tpe)
  def tpe_=(tpe: Option[Type]): Unit = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean =
    typeSymbol.map(_.defines(symbol, p)).getOrElse(false)
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = {
    typeSymbol.flatMap(_.getSymbol(name, p))
  }

  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: VariableSymbol =>
      this.mods == that.mods &&
        this.name == that.name &&
        this.typeSymbol == that.typeSymbol
    case _                    =>
      false
  }
  override def toString(): String = s"Variable symbol $name"
  override def hashCode(): Int = name.hashCode * 43 +
    typeSymbol.hashCode * mods.hashCode
}

case class MethodSymbol(var mods: Flags, var name: Name,
  var params: List[Symbol],
  var ret: Option[Symbol],
  var tpe: Option[Type],
  var owner: Option[Symbol])
  extends TermSymbol {

  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: MethodSymbol   =>
      this.name == that.name &&
        this.tpe == that.tpe &&
        this.ret == that.ret
    case _                    =>
      false
  }
  override def toString(): String = s"Method symbol: $name"
  override def hashCode(): Int = name.hashCode * 43 + (41 * tpe.hashCode *
    ret.hashCode)
}


case class ScopeSymbol(var owner: Option[Symbol]) extends Symbol {
  var name: Name = noname
  var mods: Flags = noflags
  var tpe: Option[Type] = None

  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: ScopeSymbol    =>
      this.name == that.name &&
        this.tpe == that.tpe
    case _                    =>
      false
  }
  override def toString(): String = s"Scope symbol"
  override def hashCode(): Int = name.hashCode * 43 + tpe.hashCode
}

object VoidSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(VoidType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("void")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???



  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None

  override def toString(): String = s"Void Symbol"
}
