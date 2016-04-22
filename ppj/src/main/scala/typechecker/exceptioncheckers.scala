package ch.usi.inf.l3.sana.ppj.typechecker

import ch.usi.inf.l3.sana
import sana.robustj
import sana.tiny
import sana.ppj

import robustj.typechecker.{ExceptionHandlingCheckerComponent,
                           HandledException}
import tiny.dsl._

// ASTs
import ppj.ast._

@component(tree, handledExceptions)
trait SynchronizedExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (sync: SynchronizedApi) => {
    val he = check((sync.expr, handledExceptions))
    check((sync.block, he))
  }
}

