package ch.usi.inf.l3.sana.arrooj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.calcj.types.IntType
import sana.primj.types.MethodType
import sana.primj.symbols.MethodSymbol
import sana.ooj.modifiers._
import sana.primj.modifiers._

trait SymbolUtils extends sana.ooj.symbols.SymbolUtils {
  def arrayLengthSymbol: Symbol = {
    val mods   = PUBLIC_ACC | FINAL
    val name   = Name("length")
    val params = Nil
    val ret    = getSymbol(IntType)
    val tpe    = Some(MethodType(IntType, params))
    MethodSymbol(mods, name, params, ret, tpe, None)
  }

  def mkArraySymbol(componentSymbol: Symbol): ArraySymbol =
    new ArraySymbolImpl(componentSymbol,
      objectClassSymbol, List(arrayLengthSymbol))
}

object SymbolUtils extends SymbolUtils

