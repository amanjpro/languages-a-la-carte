package ch.usi.inf.l3.sana.modulej.symbols.augmenters

import ch.usi.inf.l3.sana
import sana.primj.symbols.VariableSymbol
import sana.calcj.ast.LiteralApi



trait AugmentedVariableSymbol {

  def symbol: VariableSymbol


  def compiledRHSLiteral: Option[LiteralApi] =
    symbol.attributes.get('compiledRHSLiteral)
      .map(_.asInstanceOf[LiteralApi])

  def compiledRHSLiteral_=(lit: LiteralApi) =
    symbol.attributes = symbol.attributes + ('compiledRHSLiteral -> lit)
}


