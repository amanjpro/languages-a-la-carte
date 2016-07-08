/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
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

package ch.usi.inf.l3.sana.primj.typechecker

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.CheckerComponent
import tiny.dsl._
import tiny.ast._
import primj.ast.Implicits._
import calcj.ast._
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast._
import primj.ast.TreeUtils
import primj.symbols.MethodSymbol
import tiny.modifiers.Flags
import tiny.symbols.Symbol
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._


trait ShapeCheckerComponent extends CheckerComponent[Tree] {
  def check: Tree => Unit
}


@component
trait BlockShapeCheckerComponent extends ShapeCheckerComponent {
  (block: BlockApi)          => {
    block.stmts.foreach { tree =>
      if(!isValidStmt(tree)) {
        error(BAD_STATEMENT,
          "", "", tree.pos)
      } else ()
      check(tree)
    }
  }

  protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

}


@component
trait IfShapeCheckerComponent extends ShapeCheckerComponent {
  (ifelse: IfApi)            => {
    check(ifelse.cond)
    check(ifelse.thenp)
    check(ifelse.elsep)
    if(!isValidStmt(ifelse.thenp)) {
      error(BAD_STATEMENT,
        ifelse.thenp.toString, "a statement", ifelse.thenp.pos)
    } else ()
    if(!isValidStmt(ifelse.elsep)) {
      error(BAD_STATEMENT,
        ifelse.elsep.toString, "a statement", ifelse.elsep.pos)
    } else ()
    if(!isValidExpr(ifelse.cond)) {
      error(BAD_EXPRESSION,
        ifelse.cond.toString, "an expression", ifelse.cond.pos)
    } else ()
  }

  protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)

}


@component
trait WhileShapeCheckerComponent extends ShapeCheckerComponent {
  (wile: WhileApi)            => {
    check(wile.cond)
    check(wile.body)

    if(!isValidStmt(wile.body)) {
      error(BAD_STATEMENT,
        wile.body.toString, "a statement", wile.body.pos)
    } else ()
    if(!isValidExpr(wile.cond)) {
      error(BAD_EXPRESSION,
        wile.cond.toString, "a statement", wile.cond.pos)
    } else ()
  }

  protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)

}


@component
trait ForShapeCheckerComponent extends ShapeCheckerComponent {
  (forloop: ForApi)            => {
    forloop.inits.foreach(check(_))
    check(forloop.cond)
    forloop.steps.foreach(check(_))
    check(forloop.body)

    isValidInitStatements(forloop)
    forloop.steps.foreach { step =>
      if(!TreeUtils.isValidStatement(step))
        error(BAD_STATEMENT,
          step.toString, "a statement", step.pos)
      else ()
    }

    if(!isValidStmt(forloop.body)) {
      error(BAD_STATEMENT,
        forloop.body.toString, "a statement", forloop.body.pos)
    } else ()
    if(!isValidExpr(forloop.cond) || forloop.cond == NoTree) {
      error(BAD_EXPRESSION,
        forloop.cond.toString, "a statement", forloop.cond.pos)
    } else ()
  }

  protected def allValDefsOrNone(trees: List[Tree]): Boolean = {
    val valdefs = trees.filter(_.isInstanceOf[ValDefApi])
    valdefs.size == trees.size || valdefs.size == 0
  }

  protected def isValidInitStatements(forloop: ForApi): Unit = {
    if(!allValDefsOrNone(forloop.inits))
      error(UNEXPECTED_TREE,
        forloop.toString, "an expression", forloop.pos)
    else {
      forloop.inits.foreach { init =>
        if(!isValDefOrStatementExpression(init)) {
          error(UNEXPECTED_TREE, init.toString,
                        "", init.pos)
        }
      }
    }
  }


  protected def isValDefOrStatementExpression(t: Tree): Boolean =
    TreeUtils.isValDefOrStatementExpression(t)

  protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)

}


@component
trait CastShapeCheckerComponent extends ShapeCheckerComponent {
  (cast: CastApi)            => {
    check(cast.expr)

    if(!isTypeUse(cast.tpt)) {
      error(TYPE_NAME_EXPECTED,
      cast.tpt.toString, "a type", cast.tpt.pos)
    } else ()
  }

  protected def isTypeUse(t: UseTree): Boolean =
    TreeUtils.isTypeUse(t)

}


@component
trait ProgramShapeCheckerComponent extends ShapeCheckerComponent {
  (prg: ProgramApi) => {
    prg.members.foreach(check(_))
  }

}
@component
trait MethodDefShapeCheckerComponent extends ShapeCheckerComponent {
  (meth: MethodDefApi)  => {
    if(!isTypeUse(meth.ret)) {
      error(TYPE_NAME_EXPECTED,
        meth.ret.toString, "a type", meth.ret.pos)
    } else ()
    meth.params.foreach(check(_))
    check(meth.body)
  }

  protected def isTypeUse(tree: UseTree): Boolean =
    TreeUtils.isTypeUse(tree)
}


@component
trait UnaryShapeCheckerComponent extends ShapeCheckerComponent {
  // postfix flag can only be set if the operator is postfix
  (unary: UnaryApi) => {
    if(unary.isPostfix && (unary.op != Inc && unary.op != Dec))
      error(BAD_STATEMENT,
        unary.toString, "a postfix operation", unary.pos)
    else ()
  }

}


@component
trait ValDefShapeCheckerComponent extends ShapeCheckerComponent {
  (valdef: ValDefApi) => {
    if(!isTypeUse(valdef.tpt)) {
      // TODO: Better error message
      error(TYPE_NAME_EXPECTED,
        valdef.tpt.toString, "a type", valdef.tpt.pos)
    } else ()

    if(!sensibleParamFlag(valdef.mods, valdef.owner))
        // TODO: Better error message
        error(PARAM_OWNED_BY_NON_METHOD,
          valdef.toString, "an expression", valdef.pos)

    // val enclMeth = SymbolUtils.enclosingMethod(valdef.symbol)
    // if(enclMeth != None
    //   && !(valdef.mods.isLocalVariable || valdef.mods.isParam)) {
    //   // TODO: Better error message
    //   error(UNEXPECTED_TREE,
    //     valdef.toString, "an expression", valdef.pos)
    // } else ()
    //
    // if(enclMeth == None && !valdef.mods.isField) {
    //   // TODO: Better error message
    //   error(UNEXPECTED_TREE,
    //     valdef.toString, "an expression", valdef.pos)
    // } else ()

    if(!isSimpleExpression(valdef.rhs))
      // TODO: Better error message
      error(UNEXPECTED_TREE,
        "", "", valdef.rhs.pos)

    check(valdef.rhs)
  }

  protected def sensibleParamFlag(mods: Flags,
    sym: Option[Symbol]): Boolean = sym match {
    case Some(_: MethodSymbol) =>
      mods.isParam
    case _                     =>
      !mods.isParam
  }

  protected def isTypeUse(tree: UseTree): Boolean =
    TreeUtils.isTypeUse(tree)

  protected def isSimpleExpression(tree: Tree): Boolean =
    TreeUtils.isSimpleExpression(tree)

}
