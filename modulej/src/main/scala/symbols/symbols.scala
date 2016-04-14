package ch.usi.inf.l3.sana.modulej.symbols

import ch.usi.inf.l3.sana

import sana.ooj
import sana.tiny

import tiny.symbols.Symbol

object CompilationUnitSymbol {
  private class CompilationUnitSymbolImpl(var imports: List[Symbol],
    var module: Option[Symbol],
    var sourceName: String, var sourcePath: List[String],
    var owner: Option[Symbol]) extends CompilationUnitSymbol

  def apply(imports: List[Symbol], module: Option[Symbol], sourceName: String,
    sourcePath: List[String], owner: Option[Symbol]): CompilationUnitSymbol =
      new CompilationUnitSymbolImpl(imports, module,
        sourceName, sourcePath, owner)


  def unapply(sym: CompilationUnitSymbol):
    Option[(List[Symbol], Option[Symbol], String,
      List[String], Option[Symbol])] =
    sym match {
      case null                    => None
      case _                       =>
        Some((sym.imports, sym.module, sym.sourceName,
          sym.sourcePath, sym.owner))
    }
}


trait CompilationUnitSymbol extends ooj.symbols.CompilationUnitSymbol {
  var imports: List[Symbol]
}
