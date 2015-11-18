package ch.usi.inf.l3.sana.ooj.namers


import ch.usi.inf.l3.sana
import sana.ooj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast.{TreeCopiers => _, TreeFactories => _, _}
import tiny.symbols._
import calcj.ast.{TreeCopiers => _, TreeFactories => _, _}
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast.TreeUtils
import primj.namers.NamerComponent
import primj.symbols.{SymbolUtils => _, _}
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._
import ooj.ast._
import ooj.symbols.{SymbolUtils, ClassSymbol}
import ooj.ast.Implicits._


/*
CompilationUnit: DONE
PackageDef: DONE
ClassDef: DONE
Template: DONE
New:
Select:
This: DONE
Super: DONE -- I think
MethodDef: DONE
*/


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
          tuse::temp
      }
    }
    val body    = name(clazz.body).asInstanceOf[TemplateApi]
    TreeCopiers.copyClassDef(clazz)(body = body, parents = parents)
  }

  protected def objectClassSymbol: ClassSymbol =
    SymbolUtils.objectClassSymbol
}

@component
trait TemplateNamerComponent extends NamerComponent {
  (template: TemplateApi) => {
    val members = template.members.map(name(_))
    TreeCopiers.copyTemplate(template)(members = members)
  }
}


@component
trait MethodDefNamerComponent extends primj.namers.MethodDefNamerComponent {
  (mthd: MethodDefApi) => {
    val res  = super.apply(mthd).asInstanceOf[primj.ast.MethodDefApi]
    // INFO: a bit of hack, but works
    val res2 = TreeFactories.mkMethodDef(mthd.mods, res.ret, res.name,
      res.params, res.body)
    res2.attributes = res.attributes
    res2
  }
}


@component
trait ThisNamerComponent extends NamerComponent {
  (ths: ThisApi) => ths
}

@component
trait SuperNamerComponent extends NamerComponent {
  (spr: SuperApi) => spr
}

// Re-implement Apply, TypeUse, and Ident to support overloading
