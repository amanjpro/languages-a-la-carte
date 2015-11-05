package ch.usi.inf.l3.sana.primj.typechecker

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.CheckerComponent
import sana.dsl._
import tiny.ast._
import tiny.ast.Implicits._
import calcj.ast._
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast._
import primj.ast.TreeUtils
import primj.symbols._
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._


trait ShapeCheckerComponent extends CheckerComponent[Tree] {
  def check: Tree => Unit
}


@component
trait BlockShapeCheckerComponent extends ShapeCheckerComponent {
  (block: Block)          => {
    block.stmts.foreach { tree =>
      if(!isValidStmt(tree)) {
        error(BAD_STATEMENT,
          tree.toString, "a statement", tree.pos, tree)
      } else ()
      check(tree)
    }
  }

  protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

}


@component
trait IfShapeCheckerComponent extends ShapeCheckerComponent {
  (ifelse: If)            => {
    check(ifelse.cond)
    check(ifelse.thenp)
    check(ifelse.elsep)
    if(!isValidStmt(ifelse.thenp)) {
      error(BAD_STATEMENT,
        ifelse.thenp.toString, "a statement", ifelse.thenp.pos,
        ifelse.thenp)
    } else ()
    if(!isValidStmt(ifelse.elsep)) {
      error(BAD_STATEMENT,
        ifelse.elsep.toString, "a statement", ifelse.elsep.pos,
        ifelse.elsep)
    } else ()
    if(!isValidExpr(ifelse.cond)) {
      error(BAD_EXPRESSION,
        ifelse.cond.toString, "an expression", ifelse.cond.pos,
        ifelse.cond)
    } else ()
  }

  protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)

}


@component
trait WhileShapeCheckerComponent extends ShapeCheckerComponent {
  (wile: While)            => {
    check(wile.cond)
    check(wile.body)

    if(!isValidStmt(wile.body)) {
      error(BAD_STATEMENT,
        wile.body.toString, "a statement", wile.body.pos,
        wile.body)
    } else ()
    if(!isValidExpr(wile.cond)) {
      error(BAD_EXPRESSION,
        wile.cond.toString, "a statement", wile.cond.pos,
        wile.cond)
    } else ()
  }

  protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)

}


@component
trait ForShapeCheckerComponent extends ShapeCheckerComponent {
  (forloop: For)            => {
    forloop.inits.foreach(check(_))
    check(forloop.cond)
    forloop.steps.foreach(check(_))
    check(forloop.body)

    isValidInitStatements(forloop)
    forloop.steps.foreach { step =>
      if(!TreeUtils.isValidStatement(step))
        error(BAD_STATEMENT,
          step.toString, "a statement", step.pos,
          step)
      else ()
    }

    if(!isValidStmt(forloop.body)) {
      error(BAD_STATEMENT,
        forloop.body.toString, "a statement", forloop.body.pos,
        forloop.body)
    } else ()
    if(!isValidExpr(forloop.cond) || forloop.cond == NoTree) {
      error(BAD_EXPRESSION,
        forloop.cond.toString, "a statement", forloop.cond.pos,
        forloop.cond)
    } else ()
  }

  protected def allValDefsOrNone(trees: List[Tree]): Boolean = {
    val valdefs = trees.filter(_.isInstanceOf[ValDef])
    valdefs.size == trees.size || valdefs.size == 0
  }

  protected def isValidInitStatements(forloop: For): Unit = {
    if(!allValDefsOrNone(forloop.inits))
      error(UNEXPETED_TREE,
        forloop.toString, "an expression", forloop.pos, forloop)
    else {
      forloop.inits.foreach { init =>
        if(!TreeUtils.isValDefOrStatementExpression(init)) {
          error(UNEXPETED_TREE, init.toString,
                        "", init.pos, forloop)
        }
      }
    }
  }

  protected def isValidStmt(t: Tree): Boolean =
    TreeUtils.isValidStatement(t)

  protected def isValidExpr(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)

}


@component
trait CastShapeCheckerComponent extends ShapeCheckerComponent {
  (cast: Cast)            => {
    check(cast.expr)

    if(!isTypeUse(cast.tpt)) {
      error(TYPE_NAME_EXPECTED,
      cast.tpt.toString, "a type", cast.tpt.pos, cast.tpt)
    } else ()
  }

  protected def isTypeUse(t: UseTree): Boolean =
    TreeUtils.isTypeUse(t)

}


@component
trait ProgramShapeCheckerComponent extends ShapeCheckerComponent {
  (prg: Program) => {
    prg.members.foreach(check(_))
  }

}
@component
trait MethodDefShapeCheckerComponent extends ShapeCheckerComponent {
  (meth: MethodDef)  => {
    if(!TreeUtils.isTypeUse(meth.ret)) {
      error(TYPE_NAME_EXPECTED,
        meth.ret.toString, "a type", meth.ret.pos, meth.ret)
    } else ()
    meth.params.foreach(check(_))
    check(meth.body)
  }
}


@component
trait UnaryShapeCheckerComponent extends ShapeCheckerComponent {
  // postfix flag can only be set if the operator is postfix
  (unary: Unary) => {
    if(unary.isPostfix && (unary.op != Inc && unary.op != Dec))
      error(BAD_STATEMENT,
        unary.toString, "a postfix operation", unary.pos, unary)
    else ()
  }

}


@component
trait ValDefShapeCheckerComponent extends ShapeCheckerComponent {
  (valdef: ValDef) => {
    if(!TreeUtils.isTypeUse(valdef.tpt)) {
      // TODO: Better error message
      error(TYPE_NAME_EXPECTED,
        valdef.tpt.toString, "a type", valdef.tpt.pos, valdef.tpt)
    } else ()

    valdef.owner match {
      case Some(_: MethodSymbol) if  !valdef.mods.isParam  =>
        // TODO: Better error message
        error(UNEXPETED_TREE,
          valdef.toString, "an expression", valdef.pos, valdef)
      case _                                               =>
        ()
    }

    val enclMeth = SymbolUtils.enclosingMethod(valdef.symbol)
    if(enclMeth != None
      && !(valdef.mods.isLocalVariable || valdef.mods.isParam)) {
      // TODO: Better error message
      error(UNEXPETED_TREE,
        valdef.toString, "an expression", valdef.pos, valdef)
    } else ()

    if(enclMeth == None && !valdef.mods.isField) {
      // TODO: Better error message
      error(UNEXPETED_TREE,
        valdef.toString, "an expression", valdef.pos, valdef)
    } else ()

    if(isSimpleExpression(valdef.rhs))
      ()
    else
      // TODO: Better error message
      error(UNEXPETED_TREE,
        valdef.toString, "an expression", valdef.pos, valdef)

    check(valdef.rhs)
  }


  protected def isSimpleExpression(tree: Tree): Boolean =
    TreeUtils.isSimpleExpression(tree)

}
