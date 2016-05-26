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
import modulej.ast.TreeExtractors._
import ooj.ast.{PackageDefApi, SelectApi, ClassDefApi,
                CompilationUnitApi => OCompilationUnitApi}
import modulej.symbols.{CompilationUnitSymbol, SymbolUtils}
import tiny.symbols.{Symbol, TypeSymbol}
import ooj.symbols.PackageSymbol
import ooj.names.StdNames._
import modulej.errors.ErrorCodes._
import tiny.ast.{UseTree, SimpleUseTree, IdentApi, TypeUseApi, Tree}
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

}

@component
trait CompilationUnitNamerComponent extends NamerComponent {
  (unit: OCompilationUnitApi) => {
    unit match {
      case unit: CompilationUnitApi             =>
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
      case unit: OCompilationUnitApi            =>
        val res = TreeUpgraders.upgradeCompilationUnit(unit)
        name(res)
    }
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
    val encl = tuse.isQualified match {
      case true  => tuse.enclosing
      case false => None
    }
    res.symbol match {
      case s@Some(sym: TypeSymbol)  if isAnAccessibleType(s, encl) =>
        res
      case _                                                       =>
        error(TYPE_NOT_FOUND,
          res.toString, "a type", res.pos)
        res
    }
  }

  protected def isAnAccessibleType(sym: Option[Symbol],
    encl: Option[Symbol]): Boolean =
      SymbolUtils.isAnAccessibleType(sym, encl)

  protected def nameTypeUse(tuse: TypeUseApi): UseTree =
    identNamer.nameTypeUse(tuse)

  private[this] val identNamer = {
    val comp = this
    new TypeUseNamer {
      protected val compiler: CompilerInterface = comp.compiler
      def family(use: UseTree): UseTree =
        comp.name(use).asInstanceOf[UseTree]
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
      def family(use: UseTree): UseTree =
        comp.name(use).asInstanceOf[UseTree]
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
      family(use)
    }
  }

  protected def nameAsTypeUse(use: SimpleUseTree,
                    imports: List[(Symbol, String)]): Option[UseTree] = {
    val z: Option[UseTree] = None
    imports.foldLeft(z)((z, y) => {
      z match {
        case None       if shallUseImports(use)        =>
          val importSymbol = y._1
          val importURI    = y._2
          importSymbol.getSymbol(
            use.name, s => s.isInstanceOf[TypeSymbol]) match {
            case Some(sym)            =>
              // use.symbol = sym
              // sym.tpe.foreach(use.tpe = _)
              val fullName = s"$importURI.${use.name}"
              val newUse   = compiler.resolveNames(use.owner) {
                val res = fromQualifiedString(fullName, use)
                res.isImported = true
                res
              }.asInstanceOf[UseTree]
              // use.owner.foreach(owner =>
                  // newUse.foreach(tree => tree.owner = owner))
              // addIsQualified(newUse)
              Some(family(newUse))
              // TreeFactories.mkTypeUse(
              //     use.name, use.pos, Some(sym), use.owner)
              // tuse.attributes = use.attributes
              // Some(tuse)
            case _                    =>
              val fname = s"${importURI}.${use.name}"
              compiler.load(fname) match {
                case Some(clazz)                 =>
                  val newUse = compiler.resolveNames(use.owner) {
                    val res = fromQualifiedString(fname, use)
                    res.isImported = true
                    res
                  }.asInstanceOf[UseTree]
                  Some(family(newUse))
                case None                        =>
                  z
              }
          }
        case _                                         =>
          z
      }
    })
  }


  // protected def addIsQualified(use: Tree): Unit = use match {
  //   case Select(qual, tree)          =>
  //     addIsQualified(qual)
  //     tree.isQualified = true
  //   case _                           =>
  //     ()
  // }

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


  protected def enclosingCompilationUnit(sym:
      Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingCompilationUnit(sym)
  /**
    * Finalizes naming/typing this tree, by running the family method
    * on this tree. If in namer then it should call name, and if in
    * typer it should call typed
    */
  def family(use: UseTree): UseTree

  def shallUseImports(use: SimpleUseTree): Boolean =
    !(use.isImportQual || use.isQualified || use.isImported)


  protected def toQualifiedString(use: UseTree): String =
    TreeUtils.toQualifiedString(use)

  protected def fromQualifiedString(name: String,
        use: SimpleUseTree): UseTree =
    TreeUtils.fromQualifiedString(name) match {
      case s@Select(_, tree)              =>
        val res = TreeFactories.mkTypeUse(tree.name)
        s.pos.foreach(use.pos = _)
        res.attributes = use.attributes
        res.hasBeenNamed = false
        TreeCopiers.copySelect(s)(tree = res)
      case tree                           =>
        tree
    }

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
