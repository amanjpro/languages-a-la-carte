package ch.usi.inf.l3.sana.ooj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.primj.types.VoidType
import sana.primj
import sana.tiny.symbols.{Symbol, TermSymbol, TypeSymbol}
import sana.calcj.symbols.BooleanSymbol
import sana.calcj.types.BooleanType
import sana.primj.types.MethodType
import sana.primj.symbols.{MethodSymbol, VariableSymbol}
import sana.primj.types.VoidType
import sana.tiny.modifiers.Flags
import sana.primj.modifiers.PARAM
import sana.tiny.names.Name
import sana.tiny.names.StdNames.noname
import sana.ooj.names.StdNames.CONSTRUCTOR_NAME
import sana.ooj.modifiers._
import sana.ooj.modifiers.Ops._
import sana.ooj.types.TypeUtils




object ProgramSymbol extends Symbol {

  private val javaPackageSymbol: PackageSymbol = {
    val name  = Name("java")
    val owner = Some(ProgramSymbol)
    PackageSymbol(name, owner)
  }

  private val langPackageSymbol: PackageSymbol = {
    val name  = Name("lang")
    val owner = Some(javaPackageSymbol)
    PackageSymbol(name, owner)
  }

  private val objectClassSymbol: ClassSymbol = {
    val mods    = Flags(PUBLIC_ACC)
    val name    = Name("Object")
    val parents = Nil
    val owner   = Some(langPackageSymbol)
    val tpe     = Some(TypeUtils.objectClassType)
    val res = ClassSymbol(mods, name, parents, owner, tpe)




    // cnstr tpe:
    val cnstrTpe = Some(MethodType(VoidType, Nil))
    val cnstr = MethodSymbol(PUBLIC_ACC | CONSTRUCTOR,
      CONSTRUCTOR_NAME, Nil, Some(res),
      cnstrTpe, Some(res))

    // eqls tpe:
    val eqlsTpe = Some(MethodType(BooleanType, tpe.toList))
    val eqls = MethodSymbol(PUBLIC_ACC | noflags,
      Name("equals"), Nil, Some(BooleanSymbol), eqlsTpe, Some(res))
    val psym    = VariableSymbol(PARAM | noflags,
      Name("other"), Some(res), Some(eqls))

    eqls.params = List(psym)

    res.declare(cnstr)
    res.declare(eqls)
    res
  }


  langPackageSymbol.declare(objectClassSymbol)
  javaPackageSymbol.declare(langPackageSymbol)

  decls = (decls ++ List(javaPackageSymbol) ++
      primj.symbols.SymbolUtils.standardDefinitions)

  def name: Name = noname
  def name_=(sym: Name): Unit = ???

  def owner: Option[Symbol] = None
  def owner_=(sym: Option[Symbol]): Unit = ???

  def mods: Flags = noflags
  def mods_=(mods: Flags): Unit = ???

  def tpe: Option[Type] = None
  def tpe_=(tpe: Option[Type]): Unit = ???

  override def toString(): String = s"Package symbol: $name"
  override def hashCode(): Int = name.hashCode * 43

}

case class PackageSymbol(var name: Name,
        var owner: Option[Symbol]) extends TermSymbol {

  def mods: Flags = noflags
  def mods_=(mods: Flags): Unit = ???

  def tpe: Option[Type] = None
  def tpe_=(tpe: Option[Type]): Unit = ???

  def qualifiedName: String = {
    val ownersFullName = owner.flatMap {
      case psym: PackageSymbol => Some(psym.qualifiedName)
      case _                   => None
    }
    ownersFullName.map((oname) =>
        s"$oname.${name.asString}").getOrElse(name.asString)
  }

  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: PackageSymbol  =>
      this.qualifiedName == that.qualifiedName
    case _                    =>
      false
  }

  override def toString(): String = s"Package symbol: $name"
  override def hashCode(): Int = qualifiedName.hashCode * 43
}

case class ClassSymbol(var mods: Flags, var name: Name,
        var parents: List[ClassSymbol],
        var owner: Option[Symbol],
        var tpe: Option[Type]) extends TypeSymbol {

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
    val parentDecls =
      parents.flatMap(_.declarations).filter(sym =>
          !(decls.contains(sym) || SymbolUtils.isConstructor(sym)))
    decls ++ parentDecls
  }

  def directlyDefines(symbol: Symbol): Boolean = decls.contains(symbol)

  def getDirectlyDefinedSymbol(name: Name,
      p: Symbol => Boolean): Option[Symbol] =
    decls.find { sym =>
      sym.name == name && p(sym)
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
    val fromParents   =
      newParents.flatMap(_.getAllSymbols(name, p)).filter(!_.mods.isConstructor)
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
  override def defines(symbol: Symbol): Boolean =
    decls.contains(symbol) ||
      parents.foldLeft(false)((z, y) => y.defines(symbol) || z) ||
      owner.map { sym =>
        sym.defines(symbol)
      }.getOrElse(false)


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

  override def toString(): String = s"Class symbol: $name"
  override def hashCode(): Int = name.hashCode * 43 + tpe.hashCode
}

case class CompilationUnitSymbol(var module: Option[Symbol],
  var sourceName: String, var sourcePath: List[String],
  var owner: Option[Symbol]) extends Symbol {


  def mods: Flags = noflags
  def mods_=(tpe: Flags) = ???

  def name: Name = Name(sourceName)
  def name_=(nme: Name): Unit = ???

  def tpe: Option[Type] = None
  def tpe_=(tpe: Option[Type]): Unit = ???

  override def toString(): String = s"CompilationUnit symbol: $sourceName"
  override def hashCode(): Int = sourceName.hashCode * 43 + sourcePath.hashCode
}
