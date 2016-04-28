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

trait PackageSymbol extends TermSymbol {
  var name: Name
  var owner: Option[Symbol]
  def mods: Flags = noflags
  def mods_=(mods: Flags): Unit = ???

  def tpe: Option[Type] = None
  def tpe_=(tpe: Option[Type]): Unit = ???

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
    owner match {
      case Some(psym: PackageSymbol) =>
        psym.qualifiedNameAsList ++ List(name)
      case _                         =>
        List(name)
    }
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

trait ClassSymbol extends TypeSymbol {

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

  // Override this method to look for definitions in the parents too.
  // First local defs, then parent defs then defs in enclosing symbol
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


  // Handling scoping, does this defines a name with a predicate? If
  // not see if one of the parents defines it, lastly try the owner
  // of this symbol
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

  protected def canBeAccessedFrom(sym: Symbol): Boolean =
    !(sym.mods.isConstructor || sym.mods.isPrivateAcc)

  protected def canBeInherited(sym: Symbol): Boolean =
    !(sym.mods.isConstructor || sym.mods.isStatic)

  protected def canBeInheritedStrict(sym: Symbol): Boolean =
    !(sym.mods.isConstructor || sym.mods.isStatic || sym.mods.isPrivateAcc)

  def getInheritedSymbol(name: Name,
          p: Symbol => Boolean): Option[Symbol] = {
    parents.flatMap(_.declarations).filter(s => s.name == name && p(s)) match {
      case Nil                 => None
      case (x::xs)             => Some(x)
    }
  }

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


trait CompilationUnitSymbol extends Symbol {
  var module: Option[Symbol]
  var sourceName: String
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
