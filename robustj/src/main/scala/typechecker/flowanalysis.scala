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


import sana.dsl._
import sana.core._
import ooj.typechecker.{N, CompletenessStatus, FlowCorrectnessCheckerComponent}
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
    val benv   = env.duplicate
    val tr     =check((tri.tryClause, benv))
    val cenvs  = tri.catches.map(_ => env.duplicate)
    val z: CompletenessStatus = N
    val cr     = tri.catches.zip(cenvs).foldLeft(z){ (z, y) =>
      z.unify(check((y._1, y._2)))
    }
    cenvs.foreach(benv.unify(_))
    val fr     = tri.finallyClause.map(p => check((p, benv))).getOrElse(N)
    tr.unify(cr.unify(fr))
  }
}
