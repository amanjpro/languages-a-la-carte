package ch.usi.inf.l3.sana.modulej.namers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import sana.modulej

import tiny.core.CompilerInterface
import tiny.dsl._
import tiny.names.Name
import primj.namers.NamerComponent
import tiny.errors.ErrorReporting.{error,warning}
import modulej.ast._
import ooj.ast.{PackageDefApi, SelectApi, ClassDefApi}
import modulej.symbols.{CompilationUnitSymbol, SymbolUtils}
import tiny.symbols.{Symbol, TypeSymbol}
import ooj.symbols.PackageSymbol
import ooj.names.StdNames._
import modulej.errors.ErrorCodes._
import tiny.ast.{UseTree, SimpleUseTree, IdentApi, TypeUseApi}
import modulej.ast.Implicits._


@component
trait ClassDefNamerComponent extends ooj.namers.ClassDefNamerComponent {

  override protected def addObjectParentIfNeeded(
    clazz: ClassDefApi): List[UseTree] = {
    if(clazz.name == objectClassName &&
      enclosingPackage(clazz.symbol) == Some(langPackageSymbol))
      clazz.parents.map(parent => name(parent).asInstanceOf[UseTree])
    else super.addObjectParentIfNeeded(clazz)
  }


  protected def enclosingPackage(sym: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingPackage(sym)

}

@component
trait CompilationUnitNamerComponent extends NamerComponent {
  (unit: CompilationUnitApi) => {
    val imports = unit.imports.map { imp =>
      name(imp).asInstanceOf[ImportApi]
    }
    imports.flatMap(_.qual.symbol).foreach { sym =>
      unit.symbol.foreach(_.declare(sym))
    }
    val imports2 =
      imports.exists(_.qual.symbol == Some(langPackageSymbol)) match {
        case false              =>
          val java  = TreeFactories.mkIdent(JAVA_PACKAGE_NAME,
            owner = unit.owner)
          val lang  = TreeFactories.mkIdent(LANG_PACKAGE_NAME,
            owner = unit.owner)
          val jlang = TreeFactories.mkSelect(java, lang, owner = unit.owner)
          val imprt =
            name(TreeFactories.mkImport(jlang, true,
              owner = unit.symbol)).asInstanceOf[ImportApi]
          imprt::imports
        case true               =>
          imports
      }

    val importURIs     = imports2.flatMap(im => {
      im.qual.symbol.map(sym => (sym, toQualifiedString(im.qual)))
    })

    unit.symbol.foreach {
      case sym: CompilationUnitSymbol =>
        sym.importURIs = importURIs
      case _                          =>
    }
    val pkg     = name(unit.module).asInstanceOf[PackageDefApi]
    TreeCopiers.copyCompilationUnit(unit)(imports = imports2, module = pkg)
  }

  protected def toQualifiedString(use: UseTree): String =
    TreeUtils.toQualifiedString(use)

  protected def langPackageSymbol: PackageSymbol =
    SymbolUtils.langPackageSymbol
}

@component
trait ImportNamerComponent extends NamerComponent {
  (imprt: ImportApi)     => {
    imprt.qual.isImportQual = true
    val qual = name(imprt.qual).asInstanceOf[UseTree]
    TreeCopiers.copyImport(imprt)(qual = qual)
  }

}

@component
trait TypeUseNamerComponent extends NamerComponent {
  (tuse: TypeUseApi)          => {
    attachQualifiedNameAttribute(tuse)
    tuse.hasBeenNamed = true
    val res = nameTypeUse(tuse)
    res.symbol match {
      case Some(sym)                    =>
        res
      case None                         =>
        error(TYPE_NOT_FOUND,
          res.toString, "a type", res.pos)
        res
    }
  }

  protected def nameTypeUse(tuse: TypeUseApi): UseTree =
    identNamer.nameTypeUse(tuse)

  private[this] val identNamer = {
    val comp = this
    new TypeUseNamer {
      protected val compiler: CompilerInterface = comp.compiler
      def family(use: UseTree): UseTree = comp.name(use).asInstanceOf[UseTree]
    }
  }
  protected def attachQualifiedNameAttribute(use: UseTree): Unit =
    TreeUtils.attachQualifiedNameAttribute(use)
}

@component
trait IdentNamerComponent extends ooj.namers.IdentNamerComponent {
  (id: IdentApi)          => {
    attachQualifiedNameAttribute(id)
    super.apply(id)
  }

  override protected def nameIdent(id: IdentApi): UseTree =
    identNamer.nameIdent(id)

  private[this] val identNamer = {
    val comp = this
    new IdentNamer {
      protected val compiler: CompilerInterface = comp.compiler
      def family(use: UseTree): UseTree = comp.name(use).asInstanceOf[UseTree]
    }
  }

  protected def attachQualifiedNameAttribute(use: UseTree): Unit =
    TreeUtils.attachQualifiedNameAttribute(use)
}

trait TypeUseNamer extends SimpleUseNamer with ooj.typechecker.TypeUseNamer {

  override def nameTypeUse(tuse: TypeUseApi): UseTree = {
    val imports = enclosingCompilationUnit(tuse.owner) match {
      case Some(sym: CompilationUnitSymbol) =>
        sym.importURIs
      case _                                =>
        Nil
    }
    val tuse2  = super.nameTypeUse(tuse)
    val symbol = tuse2.symbol
    symbol match {
      case Some(sym)              =>
        tuse2
      case None                   =>
        nameAsTypeUse(tuse2) match {
          case None    if imports != Nil     =>
            tuse2 match {
              case tuse2: SimpleUseTree =>
                nameAsTypeUse(tuse2, imports).getOrElse(tuse2)
              case _                    =>
                tuse2
            }
          case Some(tuse2)                    =>
            tuse2
          case None                          =>
            tuse2
        }
        //
        // // res.foreach {
        // //   case res: SimpleUseTree => manageOwnerShip(res)
        // //   case _                  => ()
        // // }
        // res
    }
  }
}

trait IdentNamer extends ooj.namers.IdentNamer with SimpleUseNamer {
  override def nameIdent(id: IdentApi): UseTree = {
    val res = super.nameIdent(id)
    res.symbol match {
      case Some(s)       =>
        res
      case _             =>
        val otuse   = nameAsTypeUse(res)
        val imports = enclosingCompilationUnit(res.owner) match {
          case Some(sym: CompilationUnitSymbol) =>
            sym.importURIs
          case _                                =>
            Nil
        }
        val ttuse = otuse match {
          case None    if imports != Nil     =>
            nameAsTypeUse(id, imports)
          case _                             =>
            otuse
        }
        ttuse match {
          case None       =>
            nameAsTermUse(id).getOrElse(res)
          case Some(t)    =>
            t
        }
    }
  }
}

trait SimpleUseNamer {
  protected val compiler: CompilerInterface

  protected def nameAsTypeUse(use: UseTree): Option[UseTree] = {
    // id toString (using TreeUtils) and it should go
    // all the way up until it is no longer qualified
    // then name it based on that
    // then check it again, if it is still None
    for {
      fullName <- use.fullyQualifiedName
      clazz    <- compiler.load(fullName)
    } yield {
      // Can we find a class with this fully qualified name?
      family(use).asInstanceOf[UseTree]
    }
  }

  protected def nameAsTypeUse(use: SimpleUseTree,
                    imports: List[(Symbol, String)]): Option[UseTree] = {
    val z: Option[UseTree] = None
    imports.foldLeft(z)((z, y) => {
      z match {
        case None       if shallUseImports(use)        =>
          val importSymbol = y._1
          importSymbol.getSymbol(
            use.name, s => s.isInstanceOf[TypeSymbol]) match {
            case Some(sym)            =>
              use.symbol = sym
              sym.tpe.foreach(use.tpe = _)
              val tuse = TreeFactories.mkTypeUse(
                  use.name, use.pos, Some(sym), use.owner)
              tuse.attributes = use.attributes
              Some(tuse)
            case _                    =>
              val fname = s"${y._2}.${use.name}"
              compiler.load(fname) match {
                case Some(clazz)                 =>
                  val use = fromQualifiedString(fname)
                  Some(family(use))
                case None                        =>
                  z
              }
          }
        case _                                         =>
          z
      }
    })
  }

  protected def nameAsTermUse(use: UseTree): Option[UseTree] = {
    // id toString (using TreeUtils) and it should go
    // all the way up until it is no longer qualified
    // then name it based on that
    // then check it again, if it is still None
    for {
      fullName <- use.fullyQualifiedName
      // Can we find a package with this fully qualified name?
      if compiler.definesModule(fullName)
    } yield {
      val pkg1 = toPackage(fullName.split("[.]").toList)
      val pkg2 = compiler.typeCheck(rootSymbol)(pkg1)
      pkg2.symbol.foreach(use.symbol = _)
      use
    }
  }


  protected def enclosingCompilationUnit(sym: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingCompilationUnit(sym)
  /**
    * Finalizes naming/typing this tree, by running the family method
    * on this tree. If in namer it should call name, and if in typer it
    * should call typed
    */
  def family(use: UseTree): UseTree

  def shallUseImports(use: SimpleUseTree): Boolean =
    !(use.isImportQual || use.isQualified)


  protected def toQualifiedString(use: UseTree): String =
    TreeUtils.toQualifiedString(use)

  protected def fromQualifiedString(name: String): UseTree =
    TreeUtils.fromQualifiedString(name)

  protected def toPackage(names: List[String]): PackageDefApi =
    TreeUtils.toPackage(names)

  protected def rootSymbol: Option[Symbol] =
    SymbolUtils.rootSymbol

}


@component
trait SelectNamerComponent extends ooj.namers.SelectNamerComponent {
  (slct: SelectApi) => {
    attachQualifiedNameAttribute(slct)
    super.apply(slct)
  }

  protected def attachQualifiedNameAttribute(use: UseTree): Unit =
    TreeUtils.attachQualifiedNameAttribute(use)
}
