package ch.usi.inf.l3.sana.ooj.namers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.errors.ErrorReporting.{error,warning}
import tiny.symbols._
import calcj.ast.{TreeCopiers => _, _}
import primj.ast.{TreeCopiers => _, MethodDefApi => _, _}
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._
import primj.symbols.MethodSymbol
import primj.namers.SymbolAssignerComponent
import ooj.ast._
import ooj.ast.Implicits._
import ooj.symbols._



/*
CompilationUnit: DONE
PackageDef: DONE
ClassDef: DONE
Template: DONE
New: DONE
Select: DONE
This: DONE
Super: DONE
MethodDef: DONE
*/

@component(tree, owner)
trait CompilationUnitSymbolAssignerComponent extends SymbolAssignerComponent {
  (cunit: CompilationUnitApi) => {
    val sym     = CompilationUnitSymbol(None, cunit.sourceName, cunit.sourcePath)
    val pkg     = assign((cunit.module, Some(sym))).asInstanceOf[PackageDefApi]
    sym.module  = pkg.symbol
    TreeCopiers.copyCompilationUnit(cunit)(module = pkg)
  }
}

@component(tree, owner)
trait PackageDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (pkg: PackageDefApi) => {
    val sym     = PackageSymbol(pkg.name, owner)
    val members = pkg.members.map { member =>
      assign((member, Some(sym)))
    }
    pkg.symbol = sym
    owner.foreach(pkg.owner = _)
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }
}

@component(tree, owner)
trait ClassDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (clazz: ClassDefApi) => {
    val parents  = clazz.parents.map(parent =>
        assign((parent, owner)).asInstanceOf[UseTree])
    val sym      = ClassSymbol(clazz.mods, clazz.name,
      Nil, owner, None)
    val template = assign((clazz.body, Some(sym))).asInstanceOf[TemplateApi]
    clazz.symbol = sym
    owner.foreach(clazz.owner = _)
    TreeCopiers.copyClassDef(clazz)(parents = parents, body = template)
  }
}

@component(tree, owner)
trait TemplateSymbolAssignerComponent extends SymbolAssignerComponent {
  (tmpl: TemplateApi) => {
    val members  = tmpl.members.map(member =>
        assign((member, owner)))
    owner.foreach(tmpl.owner = _)
    TreeCopiers.copyTemplate(tmpl)(members = members)
  }
}

@component(tree, owner)
trait NewSymbolAssignerComponent extends SymbolAssignerComponent {
  (nu: NewApi) => {
    val app      = assign((nu.app, owner)).asInstanceOf[ApplyApi]
    owner.foreach(nu.owner = _)
    TreeCopiers.copyNew(nu)(app = app)
  }
}

@component(tree, owner)
trait SelectSymbolAssignerComponent extends SymbolAssignerComponent {
  (select: SelectApi) => {
    val qual           = assign((select.qual, owner))
    val slctdOwner     = qual.symbol
    val slctdEnclosing = owner
    slctdEnclosing.foreach(select.tree.enclosing = _)
    val slctd          =
      assign((select.tree, slctdOwner)).asInstanceOf[SimpleUseTree]
    owner.foreach(select.owner = _)
    TreeCopiers.copySelect(select)(tree = slctd, qual = qual)
  }
}

@component(tree, owner)
trait ThisSymbolAssignerComponent extends SymbolAssignerComponent {
  (ths: ThisApi) => {
    owner.foreach(ths.owner = _)
    val enclosingClass = SymbolUtils.enclosingClass(owner)
    enclosingClass.foreach(ths.enclosingClassSymbol = _)
    ths
  }
}

@component(tree, owner)
trait SuperSymbolAssignerComponent extends SymbolAssignerComponent {
  (ths: SuperApi) => {
    owner.foreach(ths.owner = _)
    val enclosingClass = SymbolUtils.enclosingClass(owner)
    enclosingClass.foreach(ths.enclosingClassSymbol = _)
    ths
  }
}

@component(tree, owner)
trait MethodDefSymbolAssignerComponent
    extends primj.namers.MethodDefSymbolAssignerComponent {

  (mthd: primj.ast.MethodDefApi)          => {
    val res = super.apply((mthd, owner))
    res match {
      case m: MethodDefApi =>
        res.symbol.foreach(_.mods = m.mods)
        res
      case _               =>
        res
    }
  }
}


// @component(tree, owner)
// trait IdentSymbolAssignerComponent extends SymbolAssignerComponent {
//   (id: Ident)          => {
//     owner.foreach(id.owner = _)
//     id
//   }
// }
//
// @component(tree, owner)
// trait TypeUseSymbolAssignerComponent extends SymbolAssignerComponent {
//   (tuse: TypeUse)          => {
//     val symbol = owner.flatMap(_.getSymbol(tuse.name,
//       _.isInstanceOf[TypeSymbol]))
//     symbol match {
//       case Some(sym)      =>
//         owner.foreach(tuse.owner = _)
//         tuse
//       case _              => tuse
//     }
//   }
// }
