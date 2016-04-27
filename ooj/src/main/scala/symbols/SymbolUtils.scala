package ch.usi.inf.l3.sana.ooj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.types.Type
import sana.tiny.names.Name
import sana.calcj.symbols._
import sana.ooj.names.StdNames
import sana.ooj.modifiers.Ops._
import sana.ooj.types.TypeUtils
import sana.tiny.symbols.{TypeSymbol, TermSymbol}
import sana.primj.symbols.{ProgramSymbol, MethodSymbol,
                           VariableSymbol, ScopeSymbol}

trait SymbolUtils extends sana.primj.symbols.SymbolUtils {

  def fullyQualifiedName(symbol: ClassSymbol): String =
    s"${packageName(symbol)}.${symbol.name}"

  def packageName(symbol: Symbol): String = symbol match {
    case pkg: PackageSymbol => pkg.qualifiedName
    case _                  => "" // TODO: Update this when needed
  }

  def enclosingCompilationUnit(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: CompilationUnitSymbol  => Some(sym)
      case sym                         => enclosingCompilationUnit(sym.owner)
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
            case _: MethodSymbol | _: VariableSymbol       => Some(sym)
            case s: ScopeSymbol  if s.mods.isStaticInit    => Some(sym)
            case _                                         => None
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

  def isAnAccessibleType(sym: Option[Symbol],
    encl: Option[Symbol]): Boolean = {
      sym match {
        case Some(_: ClassSymbol) if encl != None      =>
          val answer = for {
            enclPkg <- enclosingPackage(encl)
            symPkg  <- enclosingPackage(sym)
            symbol  <- sym
          } yield {
            symbol.mods.isPublicAcc || (enclPkg == symPkg)
          }
          answer.getOrElse(false)
        case _                                         =>
          true

      }
    }

  lazy val javaPackageSymbol: PackageSymbol =  {
    val name    = StdNames.JAVA_PACKAGE_NAME
    ProgramSymbol.getSymbol(name,
      _.isInstanceOf[PackageSymbol]).get.asInstanceOf[PackageSymbol]
  }

  lazy val langPackageSymbol: PackageSymbol = {
    val name    = StdNames.LANG_PACKAGE_NAME
    javaPackageSymbol.getSymbol(name,
      _.isInstanceOf[PackageSymbol]).get.asInstanceOf[PackageSymbol]
  }

  lazy val objectClassSymbol: ClassSymbol = {
    val name    = StdNames.OBJECT_TYPE_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  lazy val stringClassSymbol: ClassSymbol = {
    val name    = StdNames.STRING_TYPE_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  lazy val booleanClassSymbol: ClassSymbol = {
    val name    = StdNames.BOOLEAN_CLASS_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  lazy val characterClassSymbol: ClassSymbol = {
    val name    = StdNames.CHARACTER_CLASS_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  lazy val integerClassSymbol: ClassSymbol = {
    val name    = StdNames.INTEGER_CLASS_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  lazy val longClassSymbol: ClassSymbol = {
    val name    = StdNames.LONG_CLASS_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  lazy val floatClassSymbol: ClassSymbol = {
    val name    = StdNames.FLOAT_CLASS_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  lazy val doubleClassSymbol: ClassSymbol = {
    val name    = StdNames.DOUBLE_CLASS_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  def toBoxedSymbol(tpe: Symbol): Option[Symbol] = tpe match {
    case BooleanSymbol            =>
      Some(booleanClassSymbol)
    case CharSymbol               =>
      Some(characterClassSymbol)
    case IntSymbol                =>
      Some(integerClassSymbol)
    case LongSymbol               =>
      Some(longClassSymbol)
    case FloatSymbol              =>
      Some(floatClassSymbol)
    case DoubleSymbol             =>
      Some(doubleClassSymbol)
    case _                        =>
      None
  }

  def allAbstractMembers(symbol: Option[Symbol]): List[Symbol] = symbol match {
    case Some(cs: ClassSymbol)          =>
      val decls = cs.declarations.filter(_.isInstanceOf[MethodSymbol])
      val (abstracts, concretes) = decls.partition(_.mods.isAbstract)
      for {
        abs <- abstracts if !concretes.exists(s => s.tpe == abs.tpe && s.name == abs.name)
      } yield abs
    case _                              =>
      Nil
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
    def loop(mthds: List[Symbol], acc: List[Symbol]): List[Symbol] = {
      mthds match {
        case Nil                                 =>
          acc
        case (m::ms)                             =>
          val acc2 = acc.filter { sym =>
            (sym, m) match {
              case (m1: MethodSymbol, m2: MethodSymbol) =>
                !(methodCanBeApplied(m1.params.flatMap(_.tpe),
                      m2.params.flatMap(_.tpe)))
              case _                                    =>
                false
            }
          }
          val shouldNotAdd = acc.exists { sym =>
            (m, sym) match {
              case (m1: MethodSymbol, m2: MethodSymbol) =>
                methodCanBeApplied(m1.params.flatMap(_.tpe),
                        m2.params.flatMap(_.tpe))
              case _                                    =>
                false
            }
          }

          if(!shouldNotAdd)
            loop(ms, m::acc2)
          else
            loop(ms, acc2)
      }
    }
    loop(symbols, Nil)
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

  def areInTheSamePackages(sym1: Symbol, sym2: Symbol): Boolean = {
    val r = for {
      s1 <- enclosingPackage(Some(sym1))
      s2 <- enclosingPackage(Some(sym2))
    } yield s1 == s2
    r.getOrElse(false)
  }

  def isAccessible(symbol: Symbol, from: Symbol): Boolean = {
    if(symbol.isInstanceOf[PackageSymbol]) true
    else if(symbol.mods.isPublicAcc) true
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


  def isTypeSymbol(symbol: Option[Symbol]): Boolean =
    symbol.map(_.isInstanceOf[TypeSymbol]).getOrElse(false)

  def isTermSymbol(symbol: Option[Symbol]): Boolean =
    symbol.map(_.isInstanceOf[TermSymbol]).getOrElse(false)
}

object SymbolUtils extends SymbolUtils

