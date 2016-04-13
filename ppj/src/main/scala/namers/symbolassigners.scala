package ch.usi.inf.l3.sana.ppj.namers

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj
import sana.arrayj
import sana.ppj

import tiny.dsl._
import tiny.ast.Expr
import tiny.ast.Implicits._
import primj.ast.BlockApi
import primj.namers.SymbolAssignerComponent
import ppj.ast._


@component
trait SynchronizedSymbolAssignerComponent extends SymbolAssignerComponent {
  (sync: SynchronizedApi)     => {
    val owner = sync.owner
    owner.foreach { owner =>
      sync.expr.owner = owner
      sync.block.owner = owner
    }

    val expr = assign(sync.expr).asInstanceOf[Expr]
    val block = assign(sync.block).asInstanceOf[BlockApi]
    TreeCopiers.copySynchronized(sync)(expr, block)
  }
}
