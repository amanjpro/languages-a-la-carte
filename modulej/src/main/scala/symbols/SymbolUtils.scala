package ch.usi.inf.l3.sana.modulej.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.calcj.types.IntType
import sana.ooj.symbols.ClassSymbol
import sana.primj.symbols.ProgramSymbol
import sana.ooj.modifiers._
import sana.robustj.names.StdNames

import sana.ooj.symbols.PackageSymbol

trait SymbolUtils extends sana.robustj.symbols.SymbolUtils {
  /**
   * Return the fully qualified symbols for all qualified packages of
   * this symbol. The top-most package is the head of the list.
   *
   * The result is guaranteed to be none-empty
   */
  def fullyQualifiedSymbolList(pkg: PackageSymbol): List[PackageSymbol] =
    pkg.owner match {
      case Some(ow: PackageSymbol)    =>
        fullyQualifiedSymbolList(ow) ++ List(pkg)
      case _                          => List(pkg)
    }

  def rootSymbol: Option[Symbol] =
    Some(ProgramSymbol)
}

object SymbolUtils extends SymbolUtils

