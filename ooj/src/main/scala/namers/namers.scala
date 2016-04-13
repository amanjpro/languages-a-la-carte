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
import primj.ast.{ApplyApi, BlockApi}
import ooj.ast._
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
    val parents = {
      val temp = clazz.parents.map((parent) =>
          name(parent).asInstanceOf[UseTree])
      val sym  = objectClassSymbol
      temp.exists(_.symbol == Some(sym)) match {
        case true       => temp
        case _          =>
          val tuse = TreeFactories.mkTypeUse(sym.name,
            clazz.pos,
            Some(sym),
            None,
            sym.owner)
          tuse.isInExtendsClause = true
          tuse::temp
      }
    }
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
        val qname   = packageName(csym)
        val name    = csym.name
        val psyms   = clazz.parents.flatMap(_.symbol).toSet
        val tpe     = ClassType(qname, name, psyms)
        clazz.symbol.foreach(_.tpe = Some(tpe))
        clazz.tpe = tpe
      case _                       =>
        ()
    }
    val body    = name(clazz.body).asInstanceOf[TemplateApi]
    TreeCopiers.copyClassDef(clazz)(body = body, parents = parents)
  }

  protected def objectClassSymbol: ClassSymbol =
    SymbolUtils.objectClassSymbol

  protected def packageName(symbol: ClassSymbol): String =
    SymbolUtils.packageName(symbol)
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
  (mthd: MethodDefApi) => {
    val res  = super.apply(mthd).asInstanceOf[primj.ast.MethodDefApi]
    // INFO: a bit of hack, but works
    val res2 = TreeFactories.mkMethodDef(mthd.mods, res.ret, res.name,
      res.params, res.body)
    res2.attributes = res.attributes
    res2
  }
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
    val qual    = name(select.qual)
    val slctdOwner     = qual.symbol
    slctdOwner.foreach(select.tree.owner = _)
    val tree    = name(select.tree).asInstanceOf[SimpleUseTree]
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
trait IdentNamerComponent
  extends primj.namers.IdentNamerComponent
  with IdentNamer {

  (id: IdentApi)       => {
    id.hasBeenNamed = true
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
}




trait IdentNamer {
  protected def nameIdent(original: IdentApi): SimpleUseTree = {
    val id = TreeCopiers.copyIdent(original)(name = original.name)
    // At the beginning: we treat all (Ident)s as ambiguous names.
    // Can we see any (VariableSymbol)s with this name from the current scope?
    val temp = id.owner.flatMap(_.getSymbol(id.name,
      _.isInstanceOf[VariableSymbol]))
    temp match {
      case None        =>
        // Can we see any (TypeSymbol)s with this name from the current scope?
        val temp = id.owner.flatMap(_.getSymbol(id.name,
          _.isInstanceOf[TypeSymbol]))
          temp match {
            case None        =>
              val temp = id.owner.flatMap(_.getSymbol(id.name,
                _.isInstanceOf[PackageSymbol]))
              temp match {
                case None      =>
                  // TODO: Look for imports later when we introduce
                  // them: Section 6.5.2
                  id
                case Some(sym) =>
                  id.symbol = sym
                  sym.tpe.foreach(id.tpe = _)
                  id
              }
            case Some(sym)   =>
              id.symbol = sym
              sym.tpe.foreach(id.tpe = _)
              val tuse =
                TreeFactories.mkTypeUse(id.name, id.pos, id.symbol, id.owner)
              tuse.attributes = id.attributes
              tuse
          }
      case Some(sym)   =>
        id.symbol = sym
        sym.tpe.foreach(id.tpe = _)
        id
    }
  }
}
