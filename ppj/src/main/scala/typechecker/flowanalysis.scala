package ch.usi.inf.l3.sana.ppj.typechecker

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj
import sana.arrayj
import sana.ooj
import sana.ppj
import sana.robustj

import tiny.dsl._
import tiny.ast.{Tree, Expr}
import tiny.errors.ErrorReporting.{error,warning}
import tiny.errors.ErrorCodes._
import tiny.ast.Implicits._
import primj.ast.BlockApi
import ooj.typechecker.FlowCorrectnessCheckerComponent
import ppj.ast.TreeUtils
import ppj.ast._


@component(tree, env)
trait SynchronizedFlowCorrectnessCheckerComponent
  extends FlowCorrectnessCheckerComponent {
  (sync: SynchronizedApi)     => {
    val r1 = check((sync.expr, env))
    val r2 = check((sync.block, env))
    r1.unify(r2)
  }
}
