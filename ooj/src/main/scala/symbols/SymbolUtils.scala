package ch.usi.inf.l3.sana.ooj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.ooj.names.StdNames
import sana.ooj.modifiers.Ops._
import sana.ooj.types.TypeUtils
import sana.primj.symbols.MethodSymbol

trait SymbolUtils extends sana.primj.symbols.SymbolUtils {

  def fullyQualifiedName(symbol: ClassSymbol): String =
    s"${packageName(symbol)}.${symbol.name}"

  def packageName(symbol: ClassSymbol): String = symbol.owner match {
    case Some(pkg: PackageSymbol) => pkg.qualifiedName
    case _                        => "" // TODO: Update this when needed
  }

  def enclosingClass(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: ClassSymbol  => Some(sym)
      case sym               => enclosingClass(sym.owner)
    }


  def isConstructor(symbol: Symbol): Boolean = symbol match {
    case mthd: MethodSymbol        =>
      mthd.mods.isConstructor && mthd.name == StdNames.CONSTRUCTOR_NAME
    case _                         =>
      false
  }

  def javaPackageSymbol: PackageSymbol = {
    val mods  = noflags // TODO: fix it
    val name  = Name("java")
    val owner = None
    PackageSymbol(mods, name, owner)
  }

  def langPackageSymbol: PackageSymbol = {
    val mods  = noflags // TODO: fix it
    val name  = Name("lang")
    val owner = Some(javaPackageSymbol)
    PackageSymbol(mods, name, owner)
  }

  def objectClassSymbol: ClassSymbol = {
    val mods    = noflags // TODO: fix it
    val name    = Name("Object")
    val parents = Nil
    val owner   = Some(langPackageSymbol)
    val tpe     = Some(TypeUtils.objectClassType)
    ClassSymbol(mods, name, parents, owner, tpe)
  }

}

object SymbolUtils extends SymbolUtils

