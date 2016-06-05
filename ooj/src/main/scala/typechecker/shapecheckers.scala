/*
 * Copyright (c) <2015-2016>, see CONTRIBUTERS
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

package ch.usi.inf.l3.sana.ooj.typechecker

import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.CheckerComponent
import tiny.dsl._
import tiny.ast._
import tiny.symbols._
import tiny.ast.Implicits._
import tiny.source.Position
import calcj.ast._
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast.{MethodDefApi => PMethodDefApi,
                  TreeUtils => _, ProgramApi => _, _}
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
  (mthd: PMethodDefApi) => {
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

  // protected def checkFirstStmtExplicitConstructor(id: IdentApi,
  //   pos: Option[Position]): Unit = {
  //   id.symbol.foreach { s =>
  //     if(s.mods.isConstructor)
  //       error(EXPLICIT_CONSTRUCTOR_INVOKATION_NOT_FIRST_STATEMENT,
  //         "", "", pos)
  //   }
  // }
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

  override protected def isTypeUse(tree: UseTree): Boolean =
    TreeUtils.isTypeUse(tree)
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
      if(!isTypeUse(p)) {
        error(TYPE_NAME_EXPECTED,
          p.toString, "a type", p.pos)
      } else ()
    }

    checkMods(clazz)
    check(clazz.body)
  }

  protected def checkMods(clazz: ClassDefApi): Unit = {
    if(clazz.mods.isPrivateAcc || clazz.mods.isProtectedAcc) {
      error(BAD_CLASS_MODIFIER, "", "", clazz.pos)
    }
  }

  protected def isTypeUse(tree: UseTree): Boolean =
    TreeUtils.isTypeUse(tree)
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


  override protected def isTypeUse(tree: UseTree): Boolean =
    TreeUtils.isTypeUse(tree)

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
