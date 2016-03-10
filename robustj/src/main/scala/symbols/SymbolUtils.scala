package ch.usi.inf.l3.sana.robustj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.calcj.types.IntType
import sana.ooj.symbols.ClassSymbol
import sana.ooj.modifiers._
import sana.robustj.names.StdNames

trait SymbolUtils extends sana.arrooj.symbols.SymbolUtils {
  lazy val throwableClassSymbol: ClassSymbol = {
    val name    = StdNames.THROWABLE_CLASS_NAME
    langPackageSymbol.getSymbol(name, _ => true).get.asInstanceOf[ClassSymbol]
  }

  lazy val exceptionClassSymbol: ClassSymbol = {
    val name    = StdNames.EXCEPTION_CLASS_NAME
    langPackageSymbol.getSymbol(name, _ => true).get.asInstanceOf[ClassSymbol]
  }

  lazy val runtimeExceptionClassSymbol: ClassSymbol = {
    val name    = StdNames.RUNTIME_EXCEPTION_CLASS_NAME
    langPackageSymbol.getSymbol(name, _ => true).get.asInstanceOf[ClassSymbol]
  }

  lazy val errorClassSymbol: ClassSymbol = {
    val name    = StdNames.ERROR_CLASS_NAME
    langPackageSymbol.getSymbol(name, _ => true).get.asInstanceOf[ClassSymbol]
  }
}

object SymbolUtils extends SymbolUtils

