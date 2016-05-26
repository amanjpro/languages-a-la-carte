package ch.usi.inf.l3.sana.modulej.symbols

import ch.usi.inf.l3.sana
import sana.primj.symbols.VariableSymbol
import sana.calcj.ast.LiteralApi
import augmenters._


object Implicits {
  implicit class AugmentedVariableSymbolImpl(val symbol: VariableSymbol)
    extends AugmentedVariableSymbol
}
