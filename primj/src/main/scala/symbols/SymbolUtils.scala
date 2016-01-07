package ch.usi.inf.l3.sana.primj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.primj.types.VoidType

trait SymbolUtils extends sana.calcj.symbols.SymbolUtils {
  override def getSymbol(t: Type): Option[Symbol] = t match {
    case VoidType              => Some(VoidSymbol)
    case _                     => super.getSymbol(t)
  }

  override def standardDefinitions: Set[Symbol] =
    super.standardDefinitions + VoidSymbol

  def enclosingMethod(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: MethodSymbol => Some(sym)
      case sym               => enclosingMethod(sym.owner)
    }


  def alreadyDefinedLocalVarable(owner: Option[Symbol],
    name: Name): Boolean = {
    owner match {
      case Some(owner: MethodSymbol)          =>
        owner.directlyDefinesName(name, _.isInstanceOf[VariableSymbol])
      case Some(owner: ScopeSymbol)           =>
        owner.directlyDefinesName(name, _.isInstanceOf[VariableSymbol]) ||
          alreadyDefinedLocalVarable(owner.owner, name)
      case _                                  =>
        false
    }
  }

}

object SymbolUtils extends SymbolUtils

