package ch.usi.inf.l3.sana.ppj.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.ast.Implicits._
import sana.tiny.ast.Expr
import sana.primj.ast.BlockApi



trait TreeCopiers extends sana.robustj.ast.TreeCopiers {
  def copySynchronized(template: SynchronizedApi)(
      expr: Expr = template.expr, block: BlockApi = template.block,
      pos: Option[Position] = template.pos,
      owner: Option[Symbol]  = template.owner): SynchronizedApi = {
    val res = TreeFactories.mkSynchronized(expr, block)
    copyProperties(template, res)
    res
  }
}

object TreeCopiers extends TreeCopiers
