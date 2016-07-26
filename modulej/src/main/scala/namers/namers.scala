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

  /** @see {{{ooj.namers.ClassDefNamerComponent.addObjectParentIfNeeded}}} */
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

  /** @see {{{TreeUtils.toQualifiedString}}} */
  protected def toQualifiedString(use: UseTree): String =
    TreeUtils.toQualifiedString(use)

  /** @see {{{SymbolUtils.langPackageSymbol}}} */
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

  /** @see {{{SymbolUtils.isAnAccessibleType}}} */
  protected def isAnAccessibleType(sym: Option[Symbol],
    encl: Option[Symbol]): Boolean =
      SymbolUtils.isAnAccessibleType(sym, encl)

  /**
   * Binds a name use to its definition
   *
   * @param tuse the tree to be bound
   */
  protected def nameTypeUse(tuse: TypeUseApi): UseTree =
    identNamer.nameTypeUse(tuse)

  /** An instance of an TypeUseNamer */
  private[this] val identNamer = {
    val comp = this
    new TypeUseNamer {
      protected val compiler: CompilerInterface = comp.compiler
      def family(use: UseTree): UseTree =
        comp.name(use).asInstanceOf[UseTree]
    }
  }

  /** @see {{{TreeUtils.attachQualifiedNameAttribute}}} */
  protected def attachQualifiedNameAttribute(use: UseTree): Unit =
    TreeUtils.attachQualifiedNameAttribute(use)
}

@component
trait IdentNamerComponent extends ooj.namers.IdentNamerComponent {
  (id: IdentApi)          => {
    attachQualifiedNameAttribute(id)
    super.apply(id)
  }

  /**
   * Binds a name use to its definition
   *
   * @param id the tree to be bound
   */
  override protected def nameIdent(id: IdentApi): UseTree =
    identNamer.nameIdent(id)

  /** An instance of an IdentNamer */
  private[this] val identNamer = {
    val comp = this
    new IdentNamer {
      protected val compiler: CompilerInterface = comp.compiler
      def family(use: UseTree): UseTree =
        comp.name(use).asInstanceOf[UseTree]
    }
  }

  /** @see {{{TreeUtils.attachQualifiedNameAttribute}}} */
  protected def attachQualifiedNameAttribute(use: UseTree): Unit =
    TreeUtils.attachQualifiedNameAttribute(use)
}

/**
 * This trait is used to name type-uses, the difference between this
 * and `ooj.typechecker.TypeUseNamer` is that, this one takes the list
 * of `import` statements into account when naming.
 */
trait TypeUseNamer extends SimpleUseNamer with ooj.typechecker.TypeUseNamer {

  /** @see {{{ooj.typechecker.TypeUseNamer.nameTypeUse}}} */
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

/**
 * This trait is used to name identifiers, the difference between this
 * and `ooj.namers.IdentNamer` is that, this one takes the list
 * of `import` statements into account when naming.
 */
trait IdentNamer extends ooj.namers.IdentNamer with SimpleUseNamer {
  /** {{{ooj.namers.IdentNamer.nameIdent}}} */
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

/**
 * A trait to help naming instances of `SimpleUseTree`s. This trait takes the
 * import statements into account while performing naming
 */
trait SimpleUseNamer {
  /** A reference to the compiler interface */
  protected val compiler: CompilerInterface

  /**
   * Names a tree as if it is a type-use.
   *
   * @param use the tree to be named
   */
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

  /**
   * Names a tree as if it is a type-use using a list of import statements
   *
   * @param use the tree to be named
   * @param imports a list of import statements to be used for naming
   */
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

  /**
   * Names a tree as if it is an identifier.
   *
   * @param use the tree to be named
   */
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


  /** @see {{{SymbolUtils.enclosingCompilationUnit}}} */
  protected def enclosingCompilationUnit(sym:
      Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingCompilationUnit(sym)

  /**
   * Finalizes naming/typing the given tree, by running the family method on
   * the tree. If in namer then it should call name, and if in typer it should
   * call typed.
   *
   * @param use the tree to be named/typed
   */
  def family(use: UseTree): UseTree

  /**
   * Shall import information be used while naming the given tree. Import
   * information cannot be used if:
   * <li> the tree is `selected` part of a select statement
   * <li> the tree is itself an import statement
   *
   * @param use the tree to be checked
   */
  def shallUseImports(use: SimpleUseTree): Boolean =
    !(use.isImportQual || use.isQualified || use.isImported)


  /** @see {{{TreeUtils.toQualifiedString}}} */
  protected def toQualifiedString(use: UseTree): String =
    TreeUtils.toQualifiedString(use)

  /**
   * Given a fully-qualified name and a SimpleUseTree, it returns
   * a fully qualified tree of which resembles the fully qualified
   * name but with all the attributes of the instance of the
   * SimpleUseTree. This is useful if we have an unqualified but
   * fully named SimpleUseTree and want to convert it to a
   * fully qualified tree but still retaining its attributes.
   *
   * @param name the fully qualified name of the tree of the resulted
   *             tree
   * @param use the SimpleUseTree to borrow its attributes
   */
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

  /** @see {{{TreeUtils.toPackage}}} */
  protected def toPackage(names: List[String]): PackageDefApi =
    TreeUtils.toPackage(names)

  /** @see {{{SymbolUtils.rootSymbol}}} */
  protected def rootSymbol: Option[Symbol] =
    SymbolUtils.rootSymbol

}


@component
trait SelectNamerComponent extends ooj.namers.SelectNamerComponent {
  (slct: SelectApi) => {
    attachQualifiedNameAttribute(slct)
    super.apply(slct)
  }

  /** @see {{{TreeUtils.attachQualifiedNameAttribute}}} */
  protected def attachQualifiedNameAttribute(use: UseTree): Unit =
    TreeUtils.attachQualifiedNameAttribute(use)
}
