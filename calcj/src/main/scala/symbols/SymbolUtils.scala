package ch.usi.inf.l3.sana.calcj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.calcj.types._


trait SymbolUtils {
  def getSymbol(t: Type): Option[Symbol] = t match {
    case BooleanType           => Some(BooleanSymbol)
    case IntType               => Some(IntSymbol)
    case LongType              => Some(LongSymbol)
    case CharType              => Some(CharSymbol)
    case ShortType             => Some(ShortSymbol)
    case ByteType              => Some(ByteSymbol)
    case FloatType             => Some(FloatSymbol)
    case DoubleType            => Some(DoubleSymbol)
    case _                     => None
  }


  def standardDefinitions: Set[Symbol] = Set(
      DoubleSymbol,
      FloatSymbol,
      LongSymbol,
      IntSymbol,
      ShortSymbol,
      CharSymbol,
      ByteSymbol,
      BooleanSymbol
    )
}

object SymbolUtils extends SymbolUtils

