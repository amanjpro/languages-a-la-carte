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
import sana.tiny.types.Type
import sana.primj.types.VoidType
import sana.primj
import sana.tiny.symbols.{Symbol, TermSymbol, TypeSymbol}
import sana.calcj.symbols.BooleanSymbol
import sana.calcj.types.BooleanType
import sana.primj.types.MethodType
import sana.primj.symbols.MethodSymbol
import sana.primj.types.VoidType
import sana.tiny.modifiers.Flags
import sana.primj.modifiers.PARAM
import sana.tiny.names.Name
import sana.tiny.names.StdNames.noname
import sana.ooj.names.StdNames
import sana.ooj.modifiers._
import sana.ooj.modifiers.Ops._
import sana.ooj.types.TypeUtils




// object ProgramSymbol extends Symbol {
//
//
//
//
//
//   decls = (decls ++ List(javaPackageSymbol) ++
//       primj.symbols.SymbolUtils.standardDefinitions)
//
//   def name: Name = noname
//   def name_=(sym: Name): Unit = ???
//
//   def owner: Option[Symbol] = None
//   def owner_=(sym: Option[Symbol]): Unit = ???
//
//   def mods: Flags = noflags
//   def mods_=(mods: Flags): Unit = ???
//
//   def tpe: Option[Type] = None
//   def tpe_=(tpe: Option[Type]): Unit = ???
//
//
// }
//

object PackageSymbol {
  private class PackageSymbolImpl(var name: Name,
    var owner: Option[Symbol]) extends PackageSymbol

  def apply(name: Name, owner: Option[Symbol]): PackageSymbol =
    new PackageSymbolImpl(name, owner)


  def unapply(sym: PackageSymbol): Option[(Name, Option[Symbol])] = sym match {
    case null              => None
    case _                 => Some((sym.name, sym.owner))
  }
}

/** A trait to represent symbols for packages */
trait PackageSymbol extends TermSymbol {
  def mods: Flags = noflags
  def mods_=(mods: Flags): Unit = ???

  def tpe: Option[Type] = None
  def tpe_=(tpe: Option[Type]): Unit = ???

  /** The qualified name of this `package` */
  def qualifiedName: String = {
    qualifiedNameAsList.map(_.asString).mkString(".")
  }

  /**
   * Returns the fully qualified name of this package symbol. The
   * outer most package is the head of the list.
   *
   * The list is guaranteed not to be empty
   */
  def qualifiedNameAsList: List[Name] = {
    if(name != StdNames.DEFAULT_PACKAGE_NAME)
      owner match {
        case Some(psym: PackageSymbol) =>
          psym.qualifiedNameAsList ++ List(name)
        case _                         =>
          List(name)
      }
    else Nil
  }

  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: PackageSymbol  =>
      this.qualifiedName == that.qualifiedName
    case _                    =>
      false
  }

  override def toString(): String = s"Package symbol: $qualifiedName"
  override def hashCode(): Int = qualifiedName.hashCode * 43
}

/** A trait to represent symbols for classes */
trait ClassSymbol extends TypeSymbol {

  /** The list of the symbols of the direct super-classes of this symbol */
  var parents: List[ClassSymbol]

  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: ClassSymbol    =>
      this.owner == that.owner &&
        this.parents == that.parents &&
        this.name == that.name &&
        this.tpe  == that.tpe
    case _                    =>
      false
  }


  /** All the declarations defined within this class and its parent classes. */
  override def declarations: List[Symbol] = {
    val parentDecls = {
      val res = parents.flatMap(_.declarations).filter { sym =>
        val overridden  = decls.exists(s => s.tpe == sym.tpe &&
                                           s.name == sym.name)
        val inheritable = canBeInheritedStrict(sym)
        !overridden && inheritable
      }
      res.foldLeft(Nil: List[Symbol])((z, y) => {
        if(z.exists(s => y == s && y.owner == s.owner))
          z
        else y::z
      })
    }

    decls ++ parentDecls
  }

  /**
   * Returns all the symbols that are defined by this symbol, or one of its
   * parents that have the name `name` and satisfy a predicate.
   *
   * @param name the name of the symbol
   * @param p the predicate that the symbol should satisfy
   */
  def getAllSymbols(name: Name, p: Symbol => Boolean): List[Symbol] = {
    val newParents = {
      val interfaces = parents.filter(_.mods.isInterface)
      val classes    = parents.filter(!_.mods.isInterface)
      classes match {
        case List(x, y)                     =>
          if(x == SymbolUtils.objectClassSymbol)
            List(y) ++ interfaces
          else if(y == SymbolUtils.objectClassSymbol)
            List(x) ++ interfaces
          else
            classes ++ interfaces
        case _                              =>
            classes ++ interfaces
      }
    }
    val fromParents   = {
      val res = newParents.flatMap(_.getAllSymbols(name, p)).filter { sym =>
        !sym.mods.isConstructor
      }
      res.foldLeft(Nil: List[Symbol])((z, y) => {
        if(z.exists(s => y == s && y.owner == s.owner))
          z
        else y::z
      })
    }
    val fromThis      = decls.filter(sym => sym.name == name && p(sym))
    val updatedParent = fromParents.filter(sp =>
      !fromThis.exists(st => {
        (st, sp) match {
          case (st: MethodSymbol, sp: MethodSymbol)   =>
            val thisTpe   = st.params.map(_.tpe)
            val parentTpe = sp.params.map(_.tpe)
            val r = thisTpe.zip(parentTpe).foldLeft(true)((z, y) => {
              val r = for {
                t1 <- y._1
                t2 <- y._2
              } yield t1 =:= t2
              z && r.getOrElse(false)
            })
            r
          case _                                      =>
            false
        }
      }))

    updatedParent ++ fromThis
  }

  /**
   * Returns true if one of the following is true:
   * <li> Is this the same as the passed `symbol`, and satisfies the predicate? or
   * <li> Does this symbol, define the passed `symbol`, and satisfies the predicate? or
   * <li> Does one of the parents of this symbol define the passed `symbol` and satisfies
   *      the predicate.
   *
   * @param symbol the symbol to be checked
   * @param p the predicate
   */
  def definesDirectlyOrInherits(symbol: Symbol,
              p: Symbol => Boolean): Boolean = {
      lazy val thisSym         = this == symbol && p(this)
      lazy val inThis          = decls.exists(s => s == symbol && p(s))
      lazy val inParents       =
        parents.foldLeft(false)((z, y) => y.definesDirectlyOrInherits(symbol,
                    s => p(s) && !s.mods.isConstructor) || z)
      thisSym || inThis || inParents
  }

  /**
   * Returns true if one of the following is true:
   * <li> Does this symbol, define the passed `symbol`, and satisfies the
   *      predicate? or
   * <li> Does one of the parents of this symbol define the passed `symbol` and
   *      satisfies the predicate.
   * <li> Does the owner of this symbol defines the passed `symbol` and satisfies
   *      the predicate.
   *
   * @param symbol the symbol to be checked
   * @param p the predicate
   */
  override def defines(symbol: Symbol,
              p: Symbol => Boolean): Boolean = {
      val inThis          = decls.exists(s => s == symbol && p(s))
      lazy val inParents  = parents.foldLeft(false)((z, y) => y.defines(symbol,
                    s => p(s) && canBeAccessedFrom(s)) || z)
      lazy val inOwner    = owner.map { sym =>
        sym.defines(symbol, p)
      }.getOrElse(false)
      inThis || inParents || inOwner
  }


  /**
   * Handling scoping, does this defines a name with a predicate? If
   * not see if one of the parents defines it, lastly try the owner
   * of this symbol
   * @see [[sana.tiny.symbols.Symbol.getSymbol]]
   */
  override def getSymbol(name: Name, p: Symbol => Boolean): Option[Symbol] = {
    val thisSym = decls.find { sym =>
      sym.name == name && p(sym)
    }
    thisSym match {
      case None =>
        val sym = parents.foldLeft(None:Option[Symbol])((z, y) => {
          z match {
            case None        =>
              y.getSymbol(name, s => (p(s) && !s.mods.isConstructor))
            case _           =>
              z
          }
        })
        sym match {
          case None =>
            owner.flatMap { sym =>
              sym.getSymbol(name, p)
            }
          case _    =>
            sym
        }
      case _    => thisSym
    }
  }

  /**
   * Checks if the passed symbol can be accessed from this symbol
   *
   * @param sym the symbol to be checked, `sym` may not be
   *            owned by `this` symbol.
   */
  protected def canBeAccessedFrom(sym: Symbol): Boolean =
    !(sym.mods.isConstructor || sym.mods.isPrivateAcc)

  /*
   * Checks if the passed symbol can be inherited from this symbol. The
   * different between this and `canBeInheritedStrict` is that, the latter one
   * is more strict in the sense
   * that it returns false for all private members.
   *
   * @param sym the symbol to be checked, `sym` may not be
   *            owned by `this` symbol.
   */
  // protected def canBeInherited(sym: Symbol): Boolean =
  //   !(sym.mods.isConstructor || sym.mods.isStatic)
  //


  /**
   * Checks if the passed symbol can be inherited from this symbol.
   *
   * @param sym the symbol to be checked, `sym` may not be
   *            owned by `this` symbol.
   */
  protected def canBeInheritedStrict(sym: Symbol): Boolean =
    !(sym.mods.isConstructor || sym.mods.isStatic || sym.mods.isPrivateAcc)

  /**
   * Gets the inherited symbol that has the name `name` and satisfies a predicate.
   *
   * @param name the name of the symbol we are looking for
   * @param p the predicate that the symbol should satisfy
   */
  def getInheritedSymbol(name: Name,
          p: Symbol => Boolean): Option[Symbol] = {
    parents.flatMap(_.declarations).filter(s => s.name == name && p(s)) match {
      case Nil                 => None
      case (x::xs)             => Some(x)
    }
  }

  /**
   * Checks if this symbol defines a member which overrides the given symbol.
   *
   * @param sym the symbol to be checked
   */
  def overrides(sym: Symbol): Boolean = {
    parents.exists { p =>
      (sym, p) match {
        case (sym: MethodSymbol, p: ClassSymbol)      =>
          p.defines(sym, s => {
            s match {
              case s: MethodSymbol                    =>
                (p.canBeInheritedStrict(s) &&
                  s.name == sym.name &&
                    s.params.map(_.tpe) == sym.params.map(_.tpe) &&
                      !s.mods.isAbstract)
              case _                                  =>
                false
            }
          })
        case _                                        =>
          false
      }
    }
  }


  override def toString(): String = s"Class symbol: $name"
  override def hashCode(): Int = name.hashCode * 43 + tpe.hashCode
}

object ClassSymbol {
  private class ClassSymbolImpl(var mods: Flags, var name: Name,
        var parents: List[ClassSymbol],
        var owner: Option[Symbol],
        var tpe: Option[Type]) extends ClassSymbol

  def apply(mods: Flags, name: Name,
        parents: List[ClassSymbol],
        owner: Option[Symbol],
        tpe: Option[Type]): ClassSymbol =
    new ClassSymbolImpl(mods, name, parents, owner, tpe)


  def unapply(csym: ClassSymbol): Option[(Flags, Name, List[ClassSymbol],
          Option[Symbol], Option[Type])] = csym match {
    case null                       => None
    case _                          =>
      Some((csym.mods, csym.name, csym.parents, csym.owner, csym.tpe))
  }
}

object CompilationUnitSymbol {
  private class CompilationUnitSymbolImpl(var module: Option[Symbol],
    var sourceName: String, var sourcePath: List[String],
    var owner: Option[Symbol]) extends CompilationUnitSymbol

  def apply(module: Option[Symbol], sourceName: String,
    sourcePath: List[String], owner: Option[Symbol]): CompilationUnitSymbol =
      new CompilationUnitSymbolImpl(module, sourceName, sourcePath, owner)


  def unapply(sym: CompilationUnitSymbol):
    Option[(Option[Symbol], String, List[String], Option[Symbol])] =
    sym match {
      case null                    => None
      case _                       =>
        Some((sym.module, sym.sourceName, sym.sourcePath, sym.owner))
    }
}


/**
 * A symbol to represent the symbol of a compilation unit.
 * In Java a compilation unit is a source file.
 */
trait CompilationUnitSymbol extends Symbol {
  /**
   * The symbol of the package that is in this compilation unit.
   */
  var module: Option[Symbol]
  /** The name of the source file of this compilation unit */
  var sourceName: String
  /** The path of the source file of this compilation unit */
  var sourcePath: List[String]
  var owner: Option[Symbol]

  def mods: Flags = noflags
  def mods_=(tpe: Flags) = ???

  def name: Name = Name(sourceName)
  def name_=(nme: Name): Unit = ???

  def tpe: Option[Type] = None
  def tpe_=(tpe: Option[Type]): Unit = ???

  override def toString(): String = s"CompilationUnit symbol: $sourceName"
  override def hashCode(): Int = sourceName.hashCode * 43 + sourcePath.hashCode
  override def equals(other: Any): Boolean = other match {
    case null                            => false
    case other: CompilationUnitSymbol    =>
        other.sourceName == sourceName &&
        other.sourcePath == sourcePath
    case _                               => false
  }
}
