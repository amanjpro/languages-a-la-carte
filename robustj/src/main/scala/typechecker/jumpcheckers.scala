package ch.usi.inf.l3.sana.robustj.typechecker

import ch.usi.inf.l3.sana
import sana.robustj
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._

import robustj.ast._
import brokenj.typechecker.JumpCheckerComponent

/*
Try: DONE
Catch: DONE
*/


@component(tree, encls)
trait TryJumpCheckerComponent extends JumpCheckerComponent {
  (tri: TryApi) => {
    check((tri.tryClause, encls))
    tri.catches.foreach(c => check((c, encls)))
    tri.finallyClause.foreach(f => check((f, encls)))
  }
}


@component(tree, encls)
trait CatchJumpCheckerComponent extends JumpCheckerComponent {
  (ctch: CatchApi) => {
    check((ctch.catchClause, encls))
  }
}



