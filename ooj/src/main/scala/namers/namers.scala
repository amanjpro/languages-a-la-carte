package ch.usi.inf.l3.sana.ooj.namers


import ch.usi.inf.l3.sana
import sana.ooj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.TransformationComponent
import tiny.dsl._
import tiny.ast.{TreeCopiers => _, TreeFactories => _, _}
import tiny.names.Name
import tiny.symbols._
import tiny.errors.ErrorReporting.{error,warning}
import calcj.ast.{TreeCopiers => _, TreeFactories => _, _}
import calcj.ast.operators.{Inc, Dec}
import primj.namers.NamerComponent
import primj.symbols.{SymbolUtils => _, _}
import primj.errors.ErrorCodes._
import primj.ast.{ApplyApi, BlockApi, MethodDefApi => PMethodDefApi}
import ooj.ast._
import ooj.ast.TreeExtractors._
import ooj.types.ClassType
import ooj.names.StdNames
import ooj.modifiers._
import ooj.modifiers.Ops._
import ooj.symbols.{SymbolUtils, ClassSymbol, PackageSymbol}
import ooj.ast.Implicits._


/*
CompilationUnit: DONE
PackageDef: DONE
ClassDef: DONE
Template: DONE
New: DONE
Select: DONE
This: DONE
Super: DONE -- I think
MethodDef: DONE
Apply:
*/
@component
trait ProgramNamerComponent extends NamerComponent {
  (prg: ProgramApi) => {
    val members =
      prg.members.map(x => name(x))
    TreeCopiers.copyProgram(prg)(members = members)
  }
}



@component
trait CompilationUnitNamerComponent extends NamerComponent {
  (unit: CompilationUnitApi) => {
    val pkg = name(unit.module).asInstanceOf[PackageDefApi]
    TreeCopiers.copyCompilationUnit(unit)(module = pkg)
  }
}

@component
trait PackageDefNamerComponent extends NamerComponent {
  (pkg: PackageDefApi) => {
    val newMembers =
      pkg.members.map(x => name(x).asInstanceOf[DefTree])
    TreeCopiers.copyPackageDef(pkg)(members = newMembers)
  }
}


@component
trait ClassDefNamerComponent extends NamerComponent {
  (clazz: ClassDefApi) => {
    // If there is no explicit Object parent, add it
    // implicitly
    val parents = addObjectParentIfNeeded(clazz)
    val parentSymbols = parents.flatMap(_.symbol match {
      case Some(cs: ClassSymbol) => Some(cs)
      case _                     => None
    })
    clazz.symbol.foreach(_ match {
      case cs: ClassSymbol =>
        cs.parents = parentSymbols
      case _               => ()
    })
    clazz.symbol match {
      case Some(csym: ClassSymbol) =>
        val qname   = packageName(Some(csym))
        val name    = csym.name
        val psyms   = parents.flatMap(_.symbol).toSet
        val tpe     = ClassType(qname, name, psyms)
        clazz.symbol.foreach(_.tpe = Some(tpe))
        clazz.tpe = tpe
      case _                       =>
        ()
    }
    val body    = name(clazz.body).asInstanceOf[TemplateApi]
    TreeCopiers.copyClassDef(clazz)(body = body, parents = parents)
  }

  protected def addObjectParentIfNeeded(clazz: ClassDefApi): List[UseTree] = {

    def isObjectClass(use: NamedTree): Boolean  = use match {
      case Select(_, tuse)    =>
        tuse.name == objectClassName && tuse.owner == Some(langPackageSymbol)
      case tree               =>
        tree.name == objectClassName && tree.owner == Some(langPackageSymbol)
    }

    val parents =
      clazz.parents.map(parent => name(parent).asInstanceOf[UseTree])

    parents.exists(isObjectClass(_)) match {
      case false                                =>
        val java = TreeFactories.mkIdent(javaPackageName,
          clazz.pos, owner = clazz.owner)
        val lang = TreeFactories.mkIdent(langPackageName,
          clazz.pos, owner = clazz.owner)
        val slct = TreeFactories.mkSelect(java, lang, clazz.pos,
          owner = clazz.owner)
        val obj  = TreeFactories.mkTypeUse(objectClassName,
          clazz.pos, owner = clazz.owner)
        val res = TreeFactories.mkSelect(slct, obj, clazz.pos,
          owner = clazz.owner)
        obj.isInExtendsClause = true
        val res2 = name(res).asInstanceOf[UseTree]
        res2::parents
      case true                                => parents
    }

  }

  protected def packageName(symbol: Option[Symbol]): String = {
    val res = enclosingPackage(symbol).map { sym =>
      SymbolUtils.packageName(sym)
    }
    res.getOrElse("")
  }

  protected def enclosingPackage(sym: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingPackage(sym)

  protected def javaPackageName: Name =
    StdNames.JAVA_PACKAGE_NAME

  protected def langPackageName: Name =
    StdNames.LANG_PACKAGE_NAME

  protected def langPackageSymbol: PackageSymbol =
    SymbolUtils.langPackageSymbol

  protected def objectClassName: Name =
    StdNames.OBJECT_TYPE_NAME

}

@component
trait TemplateNamerComponent extends NamerComponent {
  (template: TemplateApi) => {
    val members = template.members.map(name(_))
    TreeCopiers.copyTemplate(template)(members = members)
  }
}


@component
trait MethodDefNamerComponent extends
    primj.namers.MethodDefNamerComponent {
  (mthd: PMethodDefApi) => {
    mthd match {
      case mthd: MethodDefApi           =>
        val res  = super.apply(mthd).asInstanceOf[PMethodDefApi]
        // INFO: a bit of hack, but works
        val res1 = TreeUpgraders.upgradeMethodDef(res)
        TreeCopiers.copyMethodDef(res1)(mods = mthd.mods)
      case mthd: PMethodDefApi          =>
        val res = TreeUpgraders.upgradeMethodDef(mthd)
        name(res)
    }
  }
}

@component
trait TypeUseNamerComponent extends NamerComponent {
  (tuse: TypeUseApi)          => {
    tuse.hasBeenNamed = true
    val symbol = tuse.owner.flatMap { owner            =>
      val p = (s: Symbol) => s.isInstanceOf[TypeSymbol]
      owner match {
        case csym: ClassSymbol if tuse.isQualified     =>
          csym.getSymbol(tuse.name, { s =>
            p(s) && csym.definesDirectlyOrInherits(s, p)
          })
        case sym                                       =>
          sym.getSymbol(tuse.name, p)
      }
    }
    val encl = tuse.isQualified match {
      case true  => tuse.enclosing
      case false => None
    }
    symbol match {
      case Some(sym: TypeSymbol)  if isAnAccessibleType(symbol, encl) =>
        tuse.symbol = sym
        sym.tpe.foreach(tuse.tpe = _)
      case Some(_)                                                    =>
        error(TYPE_NAME_EXPECTED,
          tuse.toString, "a type", tuse.pos)
      case _                                                          =>
        error(TYPE_NOT_FOUND,
          tuse.toString, "a type", tuse.pos)
    }
    tuse
  }

  protected def isAnAccessibleType(sym: Option[Symbol],
    encl: Option[Symbol]): Boolean =
      SymbolUtils.isAnAccessibleType(sym, encl)

}
// @component
// trait ThisNamerComponent extends NamerComponent {
//   (ths: ThisApi) => {
//     ths.enclosingClassSymbol.foreach(ths.symbol = _)
//     ths
//   }
// }

// @component
// trait SuperNamerComponent extends NamerComponent {
//   (spr: SuperApi) => {
//     spr.enclosingClassSymbol match {
//       case Some(csym: ClassSymbol)    =>
//         csym.parents.filter(! _.mods.isInterface) match {
//           case List(s)        =>
//             spr.symbol = s
//           case List(x, y)     =>
//             if(x == SymbolUtils.objectClassSymbol) {
//               spr.symbol = y
//             } else if (y == SymbolUtils.objectClassSymbol) {
//               spr.symbol = x
//             }
//           case _              =>
//             ()
//         }
//       case _                          =>
//         ()
//     }
//     spr
//   }
// }

@component
trait SelectNamerComponent extends NamerComponent {
  (select: SelectApi) => {
    val qual           = name(select.qual)
    val slctdOwner     = qual.symbol
    slctdOwner.foreach(select.tree.owner = _)
    val tree           = name(select.tree).asInstanceOf[SimpleUseTree]
    tree.symbol.foreach(select.symbol = _)
    TreeCopiers.copySelect(select)(qual = qual, tree = tree)
  }
}

@component
trait BlockNamerComponent extends NamerComponent {
  (block: BlockApi)          => block
}

// @component
// trait NewNamerComponent extends NamerComponent {
//   (nw: NewApi) => {
//     val app     = name(nw.app).asInstanceOf[ApplyApi]
//     nw.app.symbol.foreach(nw.symbol = _)
//     TreeCopiers.copyNew(nw)(app = app)
//   }
// }

@component
trait IdentNamerComponent extends NamerComponent {

  (id: IdentApi)       => {
    // id.hasBeenNamed = true
    val res = nameIdent(id)
    res.symbol match {
      case Some(s)       =>
        res
      case _             =>
        error(NAME_NOT_FOUND,
          id.toString, "a term name", id.pos)
        res
    }
  }


  protected def nameIdent(id: IdentApi): UseTree =
    identNamer.nameIdent(id)

  private[this] val identNamer = new IdentNamer {}
}




trait IdentNamer {
  def nameIdent(original: IdentApi): UseTree = {
    val id = TreeCopiers.copyIdent(original)(name = original.name)
    // At the beginning: we treat all (Ident)s as ambiguous names.
    // Can we see any (VariableSymbol)s with this name from the current scope?
    val temp = id.owner.flatMap { owner        =>
      val p = (s: Symbol) => s.isInstanceOf[VariableSymbol]
      owner match {
        case csym: ClassSymbol if id.isQualified       =>
          csym.getSymbol(id.name, { s =>
            p(s) && csym.definesDirectlyOrInherits(s, p)
          })
        case sym                                       =>
          sym.getSymbol(id.name, p)
      }
    }
    val encl = id.isQualified match {
      case true  => id.enclosing
      case false => None
    }
    temp match {
      case None        =>
        // Can we see any (TypeSymbol)s with this name from the current scope?
        val temp = id.owner.flatMap { owner        =>
          val p = (s: Symbol) => s.isInstanceOf[TypeSymbol]
          owner match {
            case csym: ClassSymbol if id.isQualified       =>
              csym.getSymbol(id.name, { s =>
                p(s) && csym.definesDirectlyOrInherits(s, p)
              })
            case sym                                       =>
              sym.getSymbol(id.name, p)
          }
        }
        temp match {
          case s@Some(sym)     if isAnAccessibleType(s, encl) =>
            id.symbol = sym
            sym.tpe.foreach(id.tpe = _)
            val tuse =
              TreeFactories.mkTypeUse(id.name, id.pos, id.symbol, id.owner)
            tuse.attributes = id.attributes
            tuse
          case _                                              =>
            val temp = id.owner.flatMap(_.getSymbol(id.name,
              _.isInstanceOf[PackageSymbol]))
            temp match {
              case None      =>
                id
              case Some(sym) =>
                id.symbol = sym
                sym.tpe.foreach(id.tpe = _)
                id
            }
        }
      case Some(sym)   =>
        id.symbol = sym
        sym.tpe.foreach(id.tpe = _)
        id
    }
  }



  protected def isAnAccessibleType(sym: Option[Symbol],
    encl: Option[Symbol]): Boolean =
    SymbolUtils.isAnAccessibleType(sym, encl)
}
