package ch.usi.inf.l3.sana.robustj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.symbols.Symbol
import sana.tiny.modifiers.Flags
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.names.Name

object MethodSymbol {
  private class MethodSymbolImpl(var mods: Flags, var name: Name,
    var ret: Option[Symbol],
    var params: List[Symbol],
    var throwsSymbols: List[Symbol],
    var tpe: Option[Type],
    var owner: Option[Symbol]) extends MethodSymbol

  def apply(mods: Flags, name: Name, ret: Option[Symbol],
    params: List[Symbol], throwsSymbols: List[Symbol], tpe: Option[Type],
    owner: Option[Symbol]): MethodSymbol =
    new MethodSymbolImpl(mods, name, ret, params, throwsSymbols, tpe, owner)

  def unapply(sym: MethodSymbol):
    Option[(Flags, Name, Option[Symbol], List[Symbol], List[Symbol],
      Option[Type], Option[Symbol])] =
    sym match {
      case null    => None
      case _       =>
        Some((sym.mods, sym.name, sym.ret, sym.params, sym.throwsSymbols,
          sym.tpe, sym.owner))
    }
}

trait MethodSymbol extends sana.primj.symbols.MethodSymbol {
  var throwsSymbols: List[Symbol]

  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: MethodSymbol   =>
      this.throwsSymbols == that.throwsSymbols &&
        super.equals(that)
    case _                    =>
      false
  }

  override def toString(): String = s"Method symbol: $name"
  override def hashCode(): Int = name.hashCode * 43 + (41 * tpe.hashCode *
    ret.hashCode)
}
