package ch.usi.inf.l3.sana.guod.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.calcj.types.IntType
import sana.calcj.symbols._
import sana.primj.symbols.{VariableSymbol, VoidSymbol}
import sana.ooj.symbols.{ClassSymbol, PackageSymbol, CompilationUnitSymbol}
import sana.ooj.modifiers._
import sana.robustj.names.StdNames

import sana.ooj.symbols.PackageSymbol

trait SymbolUtils extends sana.modulej.symbols.SymbolUtils {
  def toFullyQualifiedTypeName(symbol: Option[Symbol]): String = symbol match {
    case Some(vsym: VariableSymbol)         =>
      toFullyQualifiedTypeName(vsym.typeSymbol)
    case Some(csym: ClassSymbol)            =>
      toFullyQualifiedTypeName(csym.owner) + csym.name
    case Some(pkg: PackageSymbol)           =>
      if(pkg.name != StdNames.DEFAULT_PACKAGE_NAME)
        toFullyQualifiedTypeName(pkg.owner) + pkg.name + "."
      else ""
    case Some(cunit: CompilationUnitSymbol) =>
      toFullyQualifiedTypeName(cunit.owner)
    case Some(sym: BooleanSymbol)           =>
      sym.name.asString
    case Some(sym: ByteSymbol)              =>
      sym.name.asString
    case Some(sym: CharSymbol)              =>
      sym.name.asString
    case Some(sym: ShortSymbol)             =>
      sym.name.asString
    case Some(sym: IntSymbol)               =>
      sym.name.asString
    case Some(sym: LongSymbol)              =>
      sym.name.asString
    case Some(sym: FloatSymbol)             =>
      sym.name.asString
    case Some(sym: DoubleSymbol)            =>
      sym.name.asString
    case Some(sym: VoidSymbol)              =>
      sym.name.asString
    case _                                  =>
      ""
  }
}

object SymbolUtils extends SymbolUtils

