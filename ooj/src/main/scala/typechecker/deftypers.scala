package ch.usi.inf.l3.sana.ooj.typechecker


import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.types._
import tiny.ast.Implicits._
import tiny.types.TypeUtils._
import tiny.symbols.{TypeSymbol, TermSymbol}
import tiny.source.Position
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.{TyperComponent, TypePromotions}
import calcj.ast.{TreeCopiers => _, _}
import calcj.types._
// import primj.symbols._
import primj.errors.ErrorCodes._
import primj.types._
import primj.ast.{TreeCopiers => _, MethodDefApi => _, _}
import primj.modifiers.Ops._
import brokenj.ast.{TreeCopiers => _, _}
import ooj.ast._
import ooj.symbols._
import ooj.types._

/*
Needed:
=======


CompilationUnit: DONE
PackageDef: DONE
Template: DONE

ClassDef: DONE
MethodDef: DONE
ValDef:




Unneeded:
=========
Select:
Ident:
TypeUse:
New:
Case:
Swtich:
Label:
Break:
Continue:
Assign:
If:
While:
Block:
For:
Ternary:
Apply:
Return:
NoTree:
Cast:
Binary:
Literal: DONE
Unary:
This:
Super:
*/


trait DefTyperComponent extends TransformationComponent[Tree, Tree] {
  def typed: Tree => Tree
}

@component
trait CompilationUnitDefTyperComponent extends DefTyperComponent {
  (unit: CompilationUnitApi) => {
    val pkg = typed(unit.module).asInstanceOf[PackageDefApi]
    TreeCopiers.copyCompilationUnit(unit)(module = pkg)
  }
}

@component
trait PackageDefDefTyperComponent extends DefTyperComponent {
  (pkg: PackageDef) => {
    val members =
      pkg.members.map(x => typed(x).asInstanceOf[DefTree])
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }
}


@component
trait ClassDefDefTyperComponent extends DefTyperComponent {
  (clazz: ClassDefApi) => {
    val body    = typed(clazz.body).asInstanceOf[TemplateApi]
    TreeCopiers.copyClassDef(clazz)(body = body)
  }


  protected def packageName(symbol: ClassSymbol): String =
    SymbolUtils.packageName(symbol)
}



@component
trait MethodDefDefTyperComponent extends DefTyperComponent {
  (mthd: MethodDefApi)          => {
    val params  = mthd.params.map(typed(_).asInstanceOf[ValDefApi])
    val tparams = params.map(_.tpe.getOrElse(ErrorType))
    val rtpe    = mthd.ret.tpe.getOrElse(ErrorType)
    val tpe = MethodType(rtpe, tparams)
    mthd.tpe = tpe
    mthd.symbol.foreach(_.tpe = Some(tpe))
    TreeCopiers.copyMethodDef(mthd)(params = params)
  }
}



@component
trait TemplateDefTyperComponent extends DefTyperComponent {
  (template: TemplateApi) => {
    val members = template.members.map(typed(_))
    TreeCopiers.copyTemplate(template)(members = members)
  }
}


@component
trait ValDefDefTyperComponent extends DefTyperComponent {
  (valdef: ValDefApi)          => {
    val rtpe    = valdef.tpt.tpe.getOrElse(ErrorType)
    valdef.tpe  = rtpe
    valdef.symbol.foreach(_.tpe = Some(rtpe))
    valdef
  }
}


