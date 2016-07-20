/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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

  /**
   * Returns the fully qualified name of a class
   *
   * @param symbol the symbol of the class
   */
  def fullyQualifiedName(symbol: ClassSymbol): String =
    s"${packageName(symbol)}.${symbol.name}"

  /**
   * Returns the fully qualified name of a package
   *
   * @param symbol the symbol of the package
   */
  def packageName(symbol: Symbol): String = symbol match {
    case pkg: PackageSymbol => pkg.qualifiedName
    case _                  => "" // TODO: Update this when needed
  }

  /**
   * Returns the compilation-unit symbol that encloses the given
   * symbol.
   *
   * @param symbol the symbol we want the symbol of its enclosing
   *               compilation-unit
   */
  def enclosingCompilationUnit(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: CompilationUnitSymbol  => Some(sym)
      case sym                         => enclosingCompilationUnit(sym.owner)
    }

  /**
   * Returns the package symbol that encloses the given symbol
   *
   * @param symbol the symbol we want the symbol of its enclosing
   *               package
   */
  def enclosingPackage(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: PackageSymbol  => Some(sym)
      case sym                 => enclosingPackage(sym.owner)
    }

  /**
   * Returns the class symbol that encloses the given symbol
   *
   * @param symbol the symbol we want the symbol of its enclosing
   *               class
   */
  def enclosingClass(symbol: Option[Symbol]): Option[Symbol] =
    symbol.flatMap {
      case sym: ClassSymbol  => Some(sym)
      case sym               => enclosingClass(sym.owner)
    }

  /**
   * Returns the non-local symbol that encloses a given symbol.
   *
   * Non-local symbols are static-initializer and methods
   *
   * @param symbol the symbol we want the symbol of its non-local enclosing
   */
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

  /**
   * Checks if a symbol is constructor
   *
   * @param symbol the symbol to be checked
   */
  def isConstructor(symbol: Symbol): Boolean = symbol match {
    case mthd: MethodSymbol        =>
      mthd.mods.isConstructor && mthd.name == StdNames.CONSTRUCTOR_NAME
    case _                         =>
      false
  }

  /**
   * Checks if a type-symbol is accessible from a context
   *
   * @param sym the type-symbol to be checked
   * @param encl the symbol of the context
   */
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

  /** The symbol of `java` package */
  lazy val javaPackageSymbol: PackageSymbol =  {
    val name    = StdNames.JAVA_PACKAGE_NAME
    ProgramSymbol.getSymbol(name,
      _.isInstanceOf[PackageSymbol]).get.asInstanceOf[PackageSymbol]
  }

  /** The symbol of `java.lang` package */
  lazy val langPackageSymbol: PackageSymbol = {
    val name    = StdNames.LANG_PACKAGE_NAME
    javaPackageSymbol.getSymbol(name,
      _.isInstanceOf[PackageSymbol]).get.asInstanceOf[PackageSymbol]
  }

  /** The symbol of `java.lang.Object` class */
  lazy val objectClassSymbol: ClassSymbol = {
    val name    = StdNames.OBJECT_TYPE_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  /** The symbol of `java.lang.String` class */
  lazy val stringClassSymbol: ClassSymbol = {
    val name    = StdNames.STRING_TYPE_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  /** The symbol of `java.lang.Boolean` class */
  lazy val booleanClassSymbol: ClassSymbol = {
    val name    = StdNames.BOOLEAN_CLASS_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  /** The symbol of `java.lang.Character` class */
  lazy val characterClassSymbol: ClassSymbol = {
    val name    = StdNames.CHARACTER_CLASS_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  /** The symbol of `java.lang.Integer` class */
  lazy val integerClassSymbol: ClassSymbol = {
    val name    = StdNames.INTEGER_CLASS_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  /** The symbol of `java.lang.Long` class */
  lazy val longClassSymbol: ClassSymbol = {
    val name    = StdNames.LONG_CLASS_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  /** The symbol of `java.lang.Float` class */
  lazy val floatClassSymbol: ClassSymbol = {
    val name    = StdNames.FLOAT_CLASS_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  /** The symbol of `java.lang.Double` class */
  lazy val doubleClassSymbol: ClassSymbol = {
    val name    = StdNames.DOUBLE_CLASS_NAME
    langPackageSymbol.getSymbol(name,
      _.isInstanceOf[ClassSymbol]).get.asInstanceOf[ClassSymbol]
  }

  /**
   * Returns the boxed symbol of a primitive symbol
   *
   * @param tpe sym symbol to be boxed
   * @return if `sym` is not primitive, then None; otherwise, the
   *         boxed symbol that is equivalent to it as per Java.
   */
  def toBoxedSymbol(sym: Symbol): Option[Symbol] = sym match {
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

  /**
   * Returns all abstract symbols defined by a class symbol or its parents.
   *
   * @param symbol the symbol of the class
   */
  def allAbstractMembers(symbol: Option[Symbol]): List[Symbol] = symbol match {
    case Some(cs: ClassSymbol)          =>
      val decls = cs.declarations.filter(_.isInstanceOf[MethodSymbol])
      val (abstracts, concretes) = decls.partition(_.mods.isAbstract)
      val res = for {
        abs <- abstracts if !concretes.exists { s =>
          (s.tpe, abs.tpe) match {
            case (Some(stpe), Some(atpe))          =>
              stpe =:= atpe && s.name == abs.name
            case _                                 =>
              false
          }
        }
      } yield abs
      if(res != Nil) {
        println(decls)
      }
      res
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
   *
   * @param symbols the list of method symbols
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


  /**
   * Checks if a method can be applied with a list of arguments.
   *
   * @param ptpes the list of the symbols of the parameters of the
   *              method to be checked
   * @param atpes the list of the symbols of the arguments passed
   *              to the method to be checked
   */
  def methodCanBeApplied(ptpes: List[Type],
                           atpes: List[Type]): Boolean = {
    if(ptpes.size != atpes.size) false
    else
      atpes.zip(ptpes).foldLeft(true)((z, y) => {
        z && (y._1 <:< y._2)
      })
  }


  /**
   * Checks if a symbol is owned by another one
   *
   * @param symbol the symbol which we check whether it is owned
   * @param owner the symbol which we check whether it is the owner
   */
  def isOwnedBy(symbol: Symbol, owner: Symbol): Boolean = {
    symbol.owner match {
      case Some(ow) if ow == owner => true
      case Some(ow)                =>
        isOwnedBy(ow, owner)
      case _                       =>
        false
    }

  }

  /**
   * Checks if two symbols are in the same package
   *
   * @param sym1 the first symbol
   * @param sym2 the second symbol
   */
  def areInTheSamePackages(sym1: Symbol, sym2: Symbol): Boolean = {
    val r = for {
      s1 <- enclosingPackage(Some(sym1))
      s2 <- enclosingPackage(Some(sym2))
    } yield s1 == s2
    r.getOrElse(false)
  }

  /**
   * Checks if a symbol is accessible from another symbol, as per Java
   *
   * @param symbol the symbol to be accessed
   * @param from the symbol in which we access `symbol` from
   */
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


  /**
   * Checks if a given symbol is a type-symbol
   *
   * @param symbol the symbol to be checked
   */
  def isTypeSymbol(symbol: Option[Symbol]): Boolean =
    symbol.map(_.isInstanceOf[TypeSymbol]).getOrElse(false)

  /**
   * Checks if a given symbol is a term-symbol
   *
   * @param symbol the symbol to be checked
   */
  def isTermSymbol(symbol: Option[Symbol]): Boolean =
    symbol.map(_.isInstanceOf[TermSymbol]).getOrElse(false)

  /**
   * Given a type, returns the symbol of this type if it is
   * primitive, String or void.
   *
   * @param t the type that we want to get its symbol
   * @return if the type is primitive, String or void, then its
   *         symbol or else None.
   */
  override def getSymbol(t: Type): Option[Symbol] = t match {
    case tpe        if tpe =:= TypeUtils.stringClassType         =>
      Some(stringClassSymbol)
    case _                                                       =>
      super.getSymbol(t)
  }
}

object SymbolUtils extends SymbolUtils
