package ch.usi.inf.l3.sana.robustj.typechecker


import ch.usi.inf.l3.sana
import sana.robustj
import sana.arrooj
import sana.arrayj
import sana.ooj
import sana.brokenj
import sana.primj
import sana.calcj
import sana.tiny


import tiny.dsl._
import tiny.core._
import ooj.typechecker.{N, B, CompletenessStatus,
                        FlowCorrectnessCheckerComponent}
import robustj.ast._




/*
Try:
Throw: DONE
Catch: DONE
*/

@component(tree, env)
trait ThrowFlowCorrectnessCheckerComponent
  extends FlowCorrectnessCheckerComponent {
  (thrw: ThrowApi) => {
    check((thrw.expr, env))
    B
  }
}


@component(tree, env)
trait CatchFlowCorrectnessCheckerComponent
  extends FlowCorrectnessCheckerComponent {
  (ctch: CatchApi) => {
    check((ctch.catchClause, env))
  }
}


@component(tree, env)
trait TryFlowCorrectnessCheckerComponent
  extends FlowCorrectnessCheckerComponent {
  (tri: TryApi) => {
    val cenvs  = tri.catches.map(_ => env.duplicate)
    val tr     =check((tri.tryClause, env))
    val z: CompletenessStatus = N
    val cr     = tri.catches.zip(cenvs) match {
      case (c::cs)        =>
        val z = check((c._1, c._2))
        cs.foldLeft(z)((z, y) => z.unify(check((y._1, y._2))))
      case _              =>
        N
    }
    if(cr == N) {
      cenvs.foreach(env.unify(_))
    }
    val res = tri.finallyClause.map(p => check((p, env)))
    res match {
      case Some(fr)            =>
        tr.unify(cr.unify(fr))
      case _                   =>
        tr.unify(cr)
    }
  }
}
