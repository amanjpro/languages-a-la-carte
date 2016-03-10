package ch.usi.inf.l3.sana.arrooj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.calcj.types.IntType
import sana.primj.symbols.VariableSymbol
import sana.ooj.modifiers._
import sana.primj.modifiers._

trait SymbolUtils extends sana.ooj.symbols.SymbolUtils {
  lazy val arrayLengthSymbol: Symbol = {
    val mods   = PUBLIC_ACC | FINAL
    val name   = Name("length")
    val tpt    = getSymbol(IntType)
    VariableSymbol(mods, name, tpt, None)
  }

  def mkArraySymbol(componentSymbol: Symbol): ArraySymbol =
    new ArraySymbolImpl(componentSymbol,
      objectClassSymbol, List(arrayLengthSymbol))
}

object SymbolUtils extends SymbolUtils

