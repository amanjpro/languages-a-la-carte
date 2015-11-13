package ch.usi.inf.l3.sana.ooj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.primj.types.VoidType

trait SymbolUtils extends sana.primj.symbols.SymbolUtils {

  def enclosingClass(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: ClassSymbol  => Some(sym)
      case sym               => enclosingClass(sym.owner)
    }

}

object SymbolUtils extends SymbolUtils

