package ch.usi.inf.l3.sana.robustj.eval

import ch.usi.inf.l3.sana
import sana.robustj
import sana.arrooj
import sana.arrayj
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.TransformationComponent
import tiny.dsl._
import ooj.eval.ConstantFoldingComponent
import robustj.ast._
import tiny.ast.Expr
import primj.ast.BlockApi





@component(tree, env)
trait MethodDefConstantFoldingComponent
  extends ConstantFoldingComponent {
  (mthd: MethodDefApi) => {
    val (body, newEnv) = constantFold((mthd.body, env))
    (TreeCopiers.copyMethodDef(mthd)(body = body.asInstanceOf[Expr]), newEnv)
  }
}




@component(tree, env)
trait TryConstantFoldingComponent extends ConstantFoldingComponent {
  (tri: TryApi) => {
    val (tryClause, env1) = constantFold((tri.tryClause, env))
    val (catches, env2)   = tri.catches.foldLeft((Nil: List[CatchApi], env1)){
      (z, init) =>
        val inits   = z._1
        val e       = z._2
        val (res, env) = constantFold((init, e))
        (inits++List(res.asInstanceOf[CatchApi]), env)
    }
    tri.finallyClause match {
      case Some(finallyClause)      =>
        val (fc, env3) = constantFold((finallyClause, env2))
        ((TreeCopiers.copyTry(tri)(tryClause = tryClause.asInstanceOf[BlockApi],
          catches, Some(fc.asInstanceOf[BlockApi])), env3))
      case _                        =>
        ((TreeCopiers.copyTry(tri)(tryClause = tryClause.asInstanceOf[BlockApi],
          catches, None), env2))
    }
  }
}

@component(tree, env)
trait ThrowConstantFoldingComponent
  extends ConstantFoldingComponent {
  (thrw: ThrowApi) => {
    val (newExpr, newEnv) = constantFold((thrw.expr, env))
    val newThrow = TreeCopiers.copyThrow(thrw)(expr = newExpr.asInstanceOf[Expr])
    (newThrow, newEnv)
  }
}


@component(tree, env)
trait CatchConstantFoldingComponent
  extends ConstantFoldingComponent {
  (ctch: CatchApi) => {
    val (catchClause, newEnv) = constantFold((ctch.catchClause, env))
    (TreeCopiers.copyCatch(ctch)(
      catchClause = catchClause.asInstanceOf[BlockApi]), newEnv)
  }
}
