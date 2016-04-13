package ch.usi.inf.l3.sana.robustj.typechecker

import ch.usi.inf.l3.sana
import sana.robustj
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.dsl._

import robustj.ast._
import brokenj.typechecker.LabelNameCheckerComponent

/*
Try: DONE
Catch: DONE
*/


@component(tree, labelNames)
trait TryLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (tri: TryApi) => {
    check((tri.tryClause, labelNames))
    tri.catches.foreach(c => check((c, labelNames)))
    tri.finallyClause.foreach(f => check((f, labelNames)))
  }
}


@component(tree, labelNames)
trait CatchLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (ctch: CatchApi) => {
    check((ctch.catchClause, labelNames))
  }
}



