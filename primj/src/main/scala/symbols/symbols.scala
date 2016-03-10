package ch.usi.inf.l3.sana.primj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.primj.types.VoidType
import sana.tiny.symbols.{Symbol, TermSymbol, TypeSymbol}
import sana.tiny.modifiers.Flags
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.names.Name
import sana.tiny.names.StdNames.noname





trait ProgramSymbol extends Symbol {
  decls = decls ++ SymbolUtils.standardDefinitions

  def name: Name = noname
  def mods: Flags = noflags
  def tpe: Option[Type] = None
  def owner: Option[Symbol] = None

  def name_=(name: Name): Unit = ???
  def mods_=(mods: Flags): Unit = ???
  def tpe_=(tpe: Option[Type]): Unit = ???
  def owner_=(owner: Option[Symbol]): Unit = ???

  override def toString(): String = s"Program symbol"
  override def hashCode(): Int = name.hashCode * 43
}

object VariableSymbol {
  private class VariableSymbolImpl(var mods: Flags, var name: Name,
    var typeSymbol: Option[Symbol], var owner: Option[Symbol])
    extends VariableSymbol

  def apply(mods: Flags, name: Name, typeSymbol: Option[Symbol],
      owner: Option[Symbol]): VariableSymbol =
        new VariableSymbolImpl(mods, name, typeSymbol, owner)

  def unapply(sym: VariableSymbol):
    Option[(Flags, Name, Option[Symbol], Option[Symbol])] = sym match {
      case null      => None
      case _         => Some((sym.mods, sym.name, sym.typeSymbol, sym.owner))
    }
}

case object ProgramSymbol extends ProgramSymbol


trait VariableSymbol extends TermSymbol {
  var mods: Flags
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

object MethodSymbol {
  private class MethodSymbolImpl(var mods: Flags, var name: Name,
    var ret: Option[Symbol],
    var params: List[Symbol],
    var tpe: Option[Type],
    var owner: Option[Symbol]) extends MethodSymbol

  def apply(mods: Flags, name: Name, ret: Option[Symbol],
    params: List[Symbol], tpe: Option[Type],
    owner: Option[Symbol]): MethodSymbol =
    new MethodSymbolImpl(mods, name, ret, params, tpe, owner)

  def unapply(sym: MethodSymbol):
    Option[(Flags, Name, Option[Symbol], List[Symbol],
      Option[Type], Option[Symbol])] =
    sym match {
      case null    => None
      case _       =>
        Some((sym.mods, sym.name, sym.ret, sym.params, sym.tpe, sym.owner))
    }
}

trait MethodSymbol extends TermSymbol {
  var mods: Flags
  var name: Name
  var params: List[Symbol]
  var ret: Option[Symbol]
  var tpe: Option[Type]
  var owner: Option[Symbol]

  override def equals(other: Any): Boolean = other match {
    case null                    => false
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


object ScopeSymbol {
  private class ScopeSymbolImpl(var owner: Option[Symbol])
    extends ScopeSymbol

  def apply(owner: Option[Symbol]): ScopeSymbol =
    new ScopeSymbolImpl(owner)

  def unapply(sym: ScopeSymbol): Option[Option[Symbol]] = sym match {
    case null        => None
    case _           => Some(sym.owner)
  }
}

trait ScopeSymbol extends Symbol {
  var owner: Option[Symbol]
  var name: Name = noname
  var mods: Flags = noflags
  var tpe: Option[Type] = None

  override def equals(other: Any): Boolean = other match {
    case null                    => false
    case that: ScopeSymbol    =>
      this.name == that.name &&
        this.tpe == that.tpe
    case _                       =>
      false
  }
  override def toString(): String = s"Scope symbol"
  override def hashCode(): Int = name.hashCode * 43 + tpe.hashCode
}

trait VoidSymbol extends TypeSymbol {
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

case object VoidSymbol extends VoidSymbol
