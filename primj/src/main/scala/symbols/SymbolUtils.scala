package ch.usi.inf.l3.sana.primj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.primj.types.VoidType

trait SymbolUtils extends sana.calcj.symbols.SymbolUtils {
  override def getSymbol(t: Type): Option[Symbol] = t match {
    case VoidType              => Some(VoidSymbol)
    case _                     => super.getSymbol(t)
  }

  def enclosingMethod(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: MethodSymbol => Some(sym)
      case sym               => enclosingMethod(sym.owner)
    }

}

object SymbolUtils extends SymbolUtils

