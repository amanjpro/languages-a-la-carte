package ch.usi.inf.l3.sana.ppj.eval

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj
import sana.arrayj
import sana.ppj
import sana.ooj

import tiny.dsl._
import tiny.ast.Expr
import tiny.ast.Implicits._
import primj.ast.BlockApi
import ooj.eval.ConstantFoldingComponent
import ppj.ast._


@component(tree, env)
trait SynchronizedConstantFoldingComponent extends ConstantFoldingComponent {
  (sync: SynchronizedApi)     => {
    val (expr, env1)  = constantFold((sync.expr, env))
    val (block, env2) = constantFold((sync.block, env1))
    val r = TreeCopiers.copySynchronized(sync)(expr.asInstanceOf[Expr],
      block.asInstanceOf[BlockApi])
    (r, env2)
  }
}
