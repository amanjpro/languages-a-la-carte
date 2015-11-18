package ch.usi.inf.l3.sana.ooj.typechecker


import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.types._
import tiny.ast.Implicits._
import tiny.types.TypeUtils._
import tiny.symbols.{TypeSymbol, TermSymbol}
import tiny.source.Position
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.{TyperComponent, TypePromotions}
import calcj.types._
// import primj.symbols._
import primj.errors.ErrorCodes._
import primj.types._
import primj.modifiers.Ops._
import ooj.ast._
import ooj.symbols._
import ooj.types._

/*
CompilationUnit: DONE
PackageDef: DONE
ClassDef:
Template:
New:
Select:
This:
Super:
MethodDef:
TypeUse:
Ident:
Apply:
*/

// Set class type information in a separate phase, then
// advance to typer

@component
trait CompilationUnitTyperComponent extends TyperComponent {
  (unit: CompilationUnitApi) => {
    val pkg = typed(unit.module).asInstanceOf[PackageDefApi]
    TreeCopiers.copyCompilationUnit(unit)(module = pkg)
  }
}

@component
trait PackageDefTyperComponent extends TyperComponent {
  (pkg: PackageDefApi) => {
    val members = pkg.members.map(member => typed(member).asInstanceOf[DefTree])
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }
}


@component
trait ClassDefTyperComponent extends TyperComponent {
  (clazz: ClassDefApi) => {
    val parents =
      clazz.parents.map((parent) => typed(parent).asInstanceOf[UseTree])
    val body    = typed(clazz.body).asInstanceOf[TemplateApi]
    clazz.symbol match {
      case Some(csym: ClassSymbol) =>
        val qname   = packageName(csym)
        val name    = csym.name
        val psyms   = parents.flatMap(_.symbol).toSet
        val tpe     = ClassType(qname, name, psyms)
        clazz.symbol.foreach(_.tpe = Some(tpe))
        clazz.tpe = tpe
      case _                       =>
        ()
    }
    TreeCopiers.copyClassDef(clazz)(body = body, parents = parents)
  }


  protected def packageName(symbol: ClassSymbol): String =
    SymbolUtils.packageName(symbol)
}
