package ch.usi.inf.l3.sana.ppj.typechecker

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj
import sana.arrayj
import sana.ppj
import sana.robustj

import tiny.dsl._
import tiny.ast.{Tree, Expr}
import tiny.errors.ErrorReporting.{error,warning}
import tiny.errors.ErrorCodes._
import tiny.ast.Implicits._
import primj.ast.BlockApi
import brokenj.typechecker.LabelNameCheckerComponent
import ppj.ast.TreeUtils
import ppj.ast._


@component(tree, labelNames)
trait SynchronizedLabelNameCheckerComponent
  extends LabelNameCheckerComponent {
  (sync: SynchronizedApi)     => {
    check((sync.expr, labelNames))
    check((sync.block, labelNames))
  }
}
