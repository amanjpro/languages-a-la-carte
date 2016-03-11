package ch.usi.inf.l3.sana.ppj.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.ast.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast.Expr
import sana.primj.ast.BlockApi



trait TreeFactories extends sana.robustj.ast.TreeFactories {
  def mkSynchronized(expr: Expr, block: BlockApi,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): SynchronizedApi = {
    val res = new Synchronized(expr, block)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res
  }
}

object TreeFactories extends TreeFactories
