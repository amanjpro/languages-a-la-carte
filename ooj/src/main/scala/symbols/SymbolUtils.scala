package ch.usi.inf.l3.sana.ooj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.ooj.names.StdNames
import sana.ooj.modifiers.Ops._
import sana.ooj.types.TypeUtils
import sana.primj.symbols.{MethodSymbol, VariableSymbol}

trait SymbolUtils extends sana.primj.symbols.SymbolUtils {

  def fullyQualifiedName(symbol: ClassSymbol): String =
    s"${packageName(symbol)}.${symbol.name}"

  def packageName(symbol: ClassSymbol): String = symbol.owner match {
    case Some(pkg: PackageSymbol) => pkg.qualifiedName
    case _                        => "" // TODO: Update this when needed
  }

  def enclosingPackage(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: PackageSymbol  => Some(sym)
      case sym                 => enclosingPackage(sym.owner)
    }

  def enclosingClass(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: ClassSymbol  => Some(sym)
      case sym               => enclosingClass(sym.owner)
    }


  def enclosingNonLocal(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap { sym =>
      sym.owner match {
        case Some(_: ClassSymbol) =>
          sym match {
            case _: MethodSymbol | _: VariableSymbol => Some(sym)
            case _                                   => None
          }
        case Some(sym)            => enclosingNonLocal(Some(sym))
        case None                 => None
      }
    }

  def isConstructor(symbol: Symbol): Boolean = symbol match {
    case mthd: MethodSymbol        =>
      mthd.mods.isConstructor && mthd.name == StdNames.CONSTRUCTOR_NAME
    case _                         =>
      false
  }

  def javaPackageSymbol: PackageSymbol =  {
    val name  = Name("java")
    ProgramSymbol.getSymbol(name, _ => true).get.asInstanceOf[PackageSymbol]
  }

  def langPackageSymbol: PackageSymbol = {
    val name  = Name("lang")
    javaPackageSymbol.getSymbol(name, _ => true).get.asInstanceOf[PackageSymbol]
  }

  def objectClassSymbol: ClassSymbol = {
    val name    = Name("Object")
    langPackageSymbol.getSymbol(name, _ => true).get.asInstanceOf[ClassSymbol]
  }

}

object SymbolUtils extends SymbolUtils

