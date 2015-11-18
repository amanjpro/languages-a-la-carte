package ch.usi.inf.l3.sana.ooj.types

import ch.usi.inf.l3.sana
import sana.ooj.symbols.SymbolUtils

trait TypeUtils {
  def objectClassType: ClassTypeApi = {
    val qual            = SymbolUtils.packageName(SymbolUtils.objectClassSymbol)
    val name            = SymbolUtils.objectClassSymbol.name
    ClassType(qual, name, Set.empty)
  }
}


object TypeUtils extends TypeUtils
