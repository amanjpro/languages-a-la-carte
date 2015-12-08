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

ClassDef: DONE




Unneeded:
=========
Template:
MethodDef:
ValDef:
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
Literal:
Unary:
This:
Super:
*/


trait TypeTyperComponent extends TransformationComponent[Tree, Tree] {
  def typed: Tree => Tree
}

@component
trait CompilationUnitTypeTyperComponent extends TypeTyperComponent {
  (unit: CompilationUnitApi) => {
    val pkg = typed(unit.module).asInstanceOf[PackageDefApi]
    TreeCopiers.copyCompilationUnit(unit)(module = pkg)
  }
}

@component
trait PackageDefTypeTyperComponent extends TypeTyperComponent {
  (pkg: PackageDefApi) => {
    val members =
      pkg.members.map(x => typed(x).asInstanceOf[DefTree])
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }
}


@component
trait ClassDefTypeTyperComponent extends TypeTyperComponent {
  (clazz: ClassDefApi) => {
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
    // val body    = typed(clazz.body).asInstanceOf[TemplateApi]
    // TreeCopiers.copyClassDef(clazz)(body = body)
    clazz
  }


  protected def packageName(symbol: ClassSymbol): String =
    SymbolUtils.packageName(symbol)
}
