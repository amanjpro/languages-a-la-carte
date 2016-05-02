package ch.usi.inf.l3.sana.oberon0.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.calcj.symbols._
import sana.ooj.names.StdNames
import sana.ooj.modifiers.Ops._
import sana.ooj.types.TypeUtils
import sana.tiny.symbols.{TypeSymbol, TermSymbol}
import sana.primj.symbols.{ProgramSymbol, MethodSymbol,
                           VariableSymbol, ScopeSymbol}

trait SymbolUtils extends sana.arrooj.symbols.SymbolUtils {

  def enclosingModule(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: ModuleSymbol  => Some(sym)
      case sym                => enclosingModule(sym.owner)
    }
}

object SymbolUtils extends SymbolUtils


