package ch.usi.inf.l3.sana.dcct.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.calcj.types._
import sana.primj.types._

trait SymbolUtils extends sana.primj.symbols.SymbolUtils {
  override def getSymbol(t: Type): Option[Symbol] = t match {
    case IntType              => Some(IndexIntSymbol)
    case _                     => super.getSymbol(t)
  }

  override def standardDefinitions: Set[Symbol] =
    Set(IndexIntSymbol)
}

object SymbolUtils extends SymbolUtils

