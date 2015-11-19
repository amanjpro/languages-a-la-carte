package ch.usi.inf.l3.sana.ooj.typechecker


import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.types.{TypeUtils => _, _}
import tiny.symbols.{TypeSymbol, TermSymbol}
import tiny.source.Position
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.{TyperComponent, TypePromotions}
import calcj.types._
// import primj.symbols._
import primj.types._
import ooj.modifiers.Ops._
import ooj.ast._
import ooj.symbols._
import ooj.types._
import ooj.ast.Implicits._
import ooj.errors.ErrorCodes._

/*
CompilationUnit: DONE
PackageDef: DONE
ClassDef: DONE
Template: DONE
MethodDef: DONE
New:
Select:
This: DONE
Super: DONE
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
    TreeCopiers.copyClassDef(clazz)(body = body, parents = parents)
  }

  protected def packageName(symbol: ClassSymbol): String =
    SymbolUtils.packageName(symbol)
}

@component
trait TemplateTyperComponent extends TyperComponent {
  (tmpl: TemplateApi) => {
    val members = tmpl.members.map(typed(_).asInstanceOf[DefTree])
    TreeCopiers.copyTemplate(tmpl)(members = members)
  }
}


// TODO: Do we need this?
@component
trait MethodDefTyperComponent
  extends primj.typechecker.MethodDefTyperComponent {
  (mthd: MethodDefApi)             => {
    super.apply(mthd)
  }
  // (mthd: MethodDefApi)          => {
  //   val tpt     = typed(mthd.ret).asInstanceOf[UseTree]
  //   val params  = mthd.params.map(typed(_).asInstanceOf[ValDefApi])
  //   val body    = typed(mthd.body).asInstanceOf[Expr]
  //   val tparams = params.map(_.tpe.getOrElse(ErrorType))
  //   val rtpe    = tpt.tpe.getOrElse(ErrorType)
  //   val btpe    = body.tpe.getOrElse(ErrorType)
  //   if(!(btpe <:< rtpe) && rtpe =/= VoidType) {
  //     error(TYPE_MISMATCH,
  //         rtpe.toString, btpe.toString, body.pos, mthd)
  //     mthd
  //   } else {
  //     // TODO: Check if all paths eventually return
  //     if(rtpe =/= VoidType && !allPathsReturn(body)) {
  //       error(MISSING_RETURN_STATEMENT,
  //         body.toString, body.toString, body.pos, mthd)
  //       mthd
  //     } else {
  //       TreeCopiers.copyMethodDef(mthd)(ret = tpt,
  //         params = params, body = body)
  //     }
  //   }
  // }


  // def allPathsReturn(expr: Tree): Boolean = TreeUtils.allPathsReturn(expr)
}


@component
trait ThisTyperComponent extends TyperComponent {
  (ths: This)                 => {
    val enclClass = ths.enclosingClassSymbol
    val owner     = ths.owner
    enclClass match {
      case None                  =>
        error(ACCESSING_THIS_OUTSIDE_A_CLASS,
              ths.toString, "", ths.pos, ths)
      case _                     =>
        ()
    }

    val enclosing = SymbolUtils.enclosingNonLocal(owner)

    enclosing.foreach { sym => sym.mods.isStatic match {
        case true                  =>
          error(ACCESSING_THIS_IN_STATIC,
                ths.toString, "", ths.pos, ths)
        case false                 =>
          ()
      }
    }
    ths
  }
}


@component
trait SuperTyperComponent extends TyperComponent {
  (spr: Super)                 => {
    val enclClass = spr.enclosingClassSymbol
    val owner     = spr.owner
    enclClass match {
      case None                                                     =>
        error(ACCESSING_SUPER_OUTSIDE_A_CLASS,
              spr.toString, "", spr.pos, spr)
      case Some(sym) if sym.tpe == Some(TypeUtils.objectClassType)  =>
        error(ACCESSING_SUPER_IN_OBJECT_CLASS,
              spr.toString, "", spr.pos, spr)
      case _                                                        =>
        ()
    }

    val enclosing = SymbolUtils.enclosingNonLocal(owner)

    enclosing.foreach { sym => sym.mods.isStatic match {
        case true                  =>
          error(ACCESSING_SUPER_IN_STATIC,
                spr.toString, "", spr.pos, spr)
        case false                 =>
          ()
      }
    }
    spr
  }
}
