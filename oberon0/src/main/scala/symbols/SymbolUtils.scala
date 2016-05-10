package ch.usi.inf.l3.sana.oberon0.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.ooj.names.StdNames
import sana.ooj.modifiers.Ops._
import sana.ooj.types.TypeUtils
import sana.calcj.types.{IntType, BooleanType}
import sana.primj.types.VoidType
import sana.tiny.symbols.{TypeSymbol, TermSymbol}
import sana.primj.symbols.{ProgramSymbol, MethodSymbol,
                           VariableSymbol, ScopeSymbol,
                           VoidSymbol}

trait SymbolUtils extends sana.arrooj.symbols.SymbolUtils {

  def enclosingModule(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: ModuleSymbol  => Some(sym)
      case sym                => enclosingModule(sym.owner)
    }


  override def getSymbol(t: Type): Option[Symbol] = t match {
    case BooleanType           => Some(BooleanSymbol)
    case IntType               => Some(IntSymbol)
    case VoidType              => Some(VoidSymbol)
    case _                     => None
  }


  override def standardDefinitions: Set[Symbol] = Set(
      VoidSymbol,
      BooleanSymbol,
      IntSymbol
    )

}

object SymbolUtils extends SymbolUtils


