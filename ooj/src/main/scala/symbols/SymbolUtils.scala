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


  // Method Overloading
  /**
   * Taking a list of method id and method type pairs, finds the most
   * specific methods.
   *
   * According to Java's spec (1.0 ed) a method is more specific than the
   * other, if all its parameters can be passed to the latter.
   */
  def mostSpecificMethods(symbols: List[Symbol]): List[Symbol] = {
    symbols.filter{ sym =>
      symbols.foldLeft(true)((z, y) => {
        if(sym == y) z
        else {
          (sym, y) match {
            case (m1: MethodSymbol, m2: MethodSymbol) =>
              methodCanBeApplied(m1.params.flatMap(_.tpe), m2.params.flatMap(_.tpe))
            case _                                    =>
              false
          }
        }
      })
    }
  }


  def methodCanBeApplied(ptpes: List[Type],
                           atpes: List[Type]): Boolean = {
    if(ptpes.size != atpes.size) false
    else
      atpes.zip(ptpes).foldLeft(true)((z, y) => {
        z && (y._1 <:< y._2)
      })
  }


  def isOwnedBy(symbol: Symbol, owner: Symbol): Boolean = {
    symbol.owner match {
      case Some(ow) if ow == owner => true
      case Some(ow)                =>
        isOwnedBy(ow, owner)
      case _                       =>
        false
    }

  }

  def isAccessible(symbol: Symbol, from: Symbol): Boolean = {
    def areInTheSamePackages(sym1: Symbol, sym2: Symbol): Boolean = {
      val r = for {
        s1 <- enclosingPackage(Some(sym1))
        s2 <- enclosingPackage(Some(sym2))
      } yield s1 == s2
      r.getOrElse(false)
    }

    if(symbol.mods.isPublicAcc) true
    else if(symbol.mods.isPrivateAcc) {
      val r = for {
        s1 <- enclosingClass(Some(symbol))
        s2 <- enclosingClass(Some(from))
      } yield {
        if(s1 == s2) true
        else {
          isOwnedBy(s2, s1) // In case symbol is an inner class
        }
      }
      r.getOrElse(false)
    } else if(symbol.mods.isProtectedAcc) {
      lazy val r = for {
        s1 <- enclosingClass(Some(symbol))
        s2 <- enclosingClass(Some(from))
        t1 <- s1.tpe
        t2 <- s2.tpe
      } yield {
        if(t2 <:< t1 || s1 == s2) true
        else isOwnedBy(s2, s1) // In case symbol is an inner class
      }
      areInTheSamePackages(symbol, from) || r.getOrElse(false)
    } else areInTheSamePackages(symbol, from)
  }
}

object SymbolUtils extends SymbolUtils

