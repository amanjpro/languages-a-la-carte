package ch.usi.inf.l3.sana.dcct.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.calcj.types._
import sana.primj.types._
import sana.dcct.types._
import sana.dcct.symbols._

trait SymbolUtils extends sana.primj.symbols.SymbolUtils {
  override def getSymbol(t: Type): Option[Symbol] = t match {
    case IntType              => Some(IndexIntSymbol)
    case CIntType             => Some(CloudIntSymbol)
    case CStringType          => Some(CloudStringSymbol)
    case CSetType             => Some(CloudSetSymbol)
    case _                     => super.getSymbol(t)
  }

  override def standardDefinitions: Set[Symbol] =
    Set(IndexIntSymbol, CloudIntSymbol, CloudIntSymbol, CloudSetSymbol)
}

object SymbolUtils extends SymbolUtils

