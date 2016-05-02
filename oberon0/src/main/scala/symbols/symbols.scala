package ch.usi.inf.l3.sana.oberon0.symbols

import ch.usi.inf.l3.sana
import sana.tiny
import sana.ooj


import tiny.names.Name
import tiny.modifiers.Flags
import tiny.modifiers.Ops.noflags
import tiny.types.Type
import tiny.symbols.{Symbol, TermSymbol, TypeSymbol}
import ooj.symbols.PackageSymbol



object ModuleSymbol {
  private class ModuleSymbolImpl(var name: Name,
    var owner: Option[Symbol]) extends ModuleSymbol

  def apply(name: Name, owner: Option[Symbol]): ModuleSymbol =
    new ModuleSymbolImpl(name, owner)


  def unapply(sym: ModuleSymbol): Option[(Name, Option[Symbol])] = sym match {
    case null              => None
    case _                 => Some((sym.name, sym.owner))
  }
}

trait ModuleSymbol extends PackageSymbol {
  override def qualifiedName: String = name.asString

  override def qualifiedNameAsList: List[Name] = List(name)

  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: ModuleSymbol   =>
      this.name == that.name
    case _                    =>
      false
  }

  override def toString(): String = s"Module symbol: $name"
  override def hashCode(): Int = name.hashCode * 43
}


trait TypeDefSymbol extends TypeSymbol {
  def mods: Flags = noflags
  def mods_=(flags: Flags): Unit = ???

  var name: Name
  var typeSymbol: Option[Symbol]
  var owner: Option[Symbol]

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
    case null                    => false
    case that: TypeDefSymbol =>
        this.name == that.name &&
        this.owner == that.owner &&
        this.typeSymbol == that.typeSymbol
    case _                    =>
      false
  }
  override def toString(): String = s"TypeDef symbol $name"
  override def hashCode(): Int = name.hashCode * 43 +
    typeSymbol.hashCode * mods.hashCode + 47 * owner.hashCode
}


object TypeDefSymbol {
  private class TypeDefSymbolImpl(var name: Name,
    var typeSymbol: Option[Symbol], var owner: Option[Symbol])
    extends TypeDefSymbol

  def apply(name: Name, typeSymbol: Option[Symbol],
      owner: Option[Symbol]): TypeDefSymbol =
        new TypeDefSymbolImpl(name, typeSymbol, owner)

  def unapply(sym: TypeDefSymbol):
    Option[(Name, Option[Symbol], Option[Symbol])] = sym match {
      case null      => None
      case _         => Some((sym.name, sym.typeSymbol, sym.owner))
    }
}
