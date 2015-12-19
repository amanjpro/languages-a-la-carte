package ch.usi.inf.l3.sana.ooj.typechecker

import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.CheckerComponent
import sana.dsl._
import tiny.ast._
import tiny.symbols._
import tiny.ast.Implicits._
import tiny.source.Position
import calcj.ast._
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast.{MethodDefApi => _, TreeUtils => _, ProgramApi => _, _}
import primj.typechecker.ShapeCheckerComponent
import ooj.symbols._
import ooj.modifiers.Ops._
import ooj.errors.ErrorCodes._
import ooj.ast._
import ooj.names.StdNames._
import ooj.ast.TreeExtractors._

@component
trait ProgramShapeCheckerComponent extends ShapeCheckerComponent {
  (prg: ProgramApi) => {
    prg.members.foreach(x => check(x))
  }
}

@component
trait MethodDefShapeCheckerComponent extends
    primj.typechecker.MethodDefShapeCheckerComponent {
  (mthd: MethodDefApi) => {
    if(isConstructor(mthd.symbol)) {
      mthd.body match {
        case Block(Nil)                    =>
          () // pass
        case Block(stmts)                  =>
          stmts.tail.foreach {
            case stmt@Apply(Select(_: ThisApi,
              id@Ident(`CONSTRUCTOR_NAME`)), _)         =>
              checkExplicitConstructor(id, stmt.pos)
            case stmt@Apply(Select(_: SuperApi,
              id@Ident(`CONSTRUCTOR_NAME`)), _)         =>
              checkExplicitConstructor(id, stmt.pos)
            case _                                  =>
              () // pass
          }
        case _                             =>
          () // pass
      }
    } else {
      mthd.body match {
        case Block(stmts)                   =>
          stmts.foreach {
            case stmt@Apply(Select(_: ThisApi,
              id@Ident(`CONSTRUCTOR_NAME`)), _)      =>
              checkExplicitConstructor(id, stmt.pos)
            case stmt@Apply(Select(_: SuperApi,
              id@Ident(`CONSTRUCTOR_NAME`)), _)      =>
              checkExplicitConstructor(id, stmt.pos)
            case _                                  =>
              () // pass
          }
        case stmt@Apply(Select(_: ThisApi,
          id@Ident(`CONSTRUCTOR_NAME`)), _)  =>
          checkExplicitConstructor(id, stmt.pos)
        case stmt@Apply(Select(_: ThisApi,
          id@Ident(`CONSTRUCTOR_NAME`)), _)  =>
          checkExplicitConstructor(id, stmt.pos)
        case _                               =>
          () // pass
      }
    }

    super.apply(mthd)
  }

  protected def checkFirstStmtExplicitConstructor(id: IdentApi,
    pos: Option[Position]): Unit = {
    id.symbol.foreach { s =>
      if(s.mods.isConstructor)
        error(EXPLICIT_CONSTRUCTOR_INVOKATION_NOT_FIRST_STATEMENT,
          "", "", pos)
    }
  }
  protected def checkExplicitConstructor(id: IdentApi,
    pos: Option[Position]): Unit = {
    id.symbol.foreach { s =>
      if(s.mods.isConstructor)
        error(EXPLICIT_CONSTRUCTOR_INVOKATION_IN_METHOD,
          "", "", pos)
    }
  }
  protected def isConstructor(sym: Option[Symbol]): Boolean =
    sym.map(s => SymbolUtils.isConstructor(s)).getOrElse(false)

  override protected def isType(tree: UseTree): Boolean =
    TreeUtils.isType(tree)
}

@component
trait PackageDefShapeCheckerComponent extends
  ShapeCheckerComponent {
  (pkg: PackageDefApi) => {
    pkg.members.foreach(check(_))
  }
}


@component
trait ClassDefShapeCheckerComponent extends
  ShapeCheckerComponent {
  (clazz: ClassDefApi) => {
    clazz.parents.foreach { p =>
      if(!isType(p)) {
        error(TYPE_NAME_EXPECTED,
          p.toString, "a type", p.pos)
      } else ()
    }

    check(clazz.body)
  }

  protected def isType(tree: UseTree): Boolean =
    TreeUtils.isType(tree)
}


@component
trait TemplateShapeCheckerComponent extends
  ShapeCheckerComponent {
  (template: TemplateApi) => {
    template.members.foreach(check(_))
  }
}

@component
trait CompilationUnitShapeCheckerComponent extends
  ShapeCheckerComponent {
  (cunit: CompilationUnitApi) => {
    check(cunit.module)
  }
}

@component
trait ValDefShapeCheckerComponent extends
  primj.typechecker.ValDefShapeCheckerComponent {


  override protected def isType(tree: UseTree): Boolean =
    TreeUtils.isType(tree)

  override protected def isSimpleExpression(tree: Tree): Boolean =
    TreeUtils.isSimpleExpression(tree)
}


@component
trait LabelShapeCheckerComponent extends
  brokenj.typechecker.LabelShapeCheckerComponent {


  override protected def canHaveLabel(stmt: Expr): Boolean =
    TreeUtils.canHaveLabel(stmt)
}

@component
trait BlockShapeCheckerComponent extends
  primj.typechecker.BlockShapeCheckerComponent {

  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)
}

@component
trait IfShapeCheckerComponent extends
  primj.typechecker.IfShapeCheckerComponent {

  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  override protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)

}

@component
trait WhileShapeCheckerComponent extends
  primj.typechecker.WhileShapeCheckerComponent {

  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  override protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)

}

@component
trait ForShapeCheckerComponent extends
  primj.typechecker.ForShapeCheckerComponent {

  override protected def isValDefOrStatementExpression(t: Tree): Boolean =
    TreeUtils.isValDefOrStatementExpression(t)

  override protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  override protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)
}

@component
trait CastShapeCheckerComponent extends
  primj.typechecker.CastShapeCheckerComponent {

  override protected def isTypeUse(t: UseTree): Boolean =
    TreeUtils.isTypeUse(t)
}
