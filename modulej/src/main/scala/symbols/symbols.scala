package ch.usi.inf.l3.sana.modulej.symbols

import ch.usi.inf.l3.sana

import sana.ooj
import sana.tiny

import tiny.symbols.Symbol

object CompilationUnitSymbol {
  private class CompilationUnitSymbolImpl(
    var importURIs: List[(Symbol, String)],
    var module: Option[Symbol],
    var sourceName: String, var sourcePath: List[String],
    var owner: Option[Symbol]) extends CompilationUnitSymbol

  def apply(importURIs: List[(Symbol, String)],
    module: Option[Symbol], sourceName: String,
    sourcePath: List[String], owner: Option[Symbol]): CompilationUnitSymbol =
      new CompilationUnitSymbolImpl(importURIs, module,
        sourceName, sourcePath, owner)


  def unapply(sym: CompilationUnitSymbol):
    Option[(List[(Symbol, String)], Option[Symbol], String,
      List[String], Option[Symbol])] =
    sym match {
      case null                    => None
      case _                       =>
        Some((sym.importURIs, sym.module, sym.sourceName,
          sym.sourcePath, sym.owner))
    }
}


trait CompilationUnitSymbol extends ooj.symbols.CompilationUnitSymbol {
  // String is fully qualified uri for this import statement
  var importURIs: List[(Symbol, String)]
}
