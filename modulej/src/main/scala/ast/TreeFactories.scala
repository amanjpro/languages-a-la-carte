package ch.usi.inf.l3.sana.modulej.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.ast.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast.UseTree



trait TreeFactories extends sana.ppj.ast.TreeFactories {
  def mkImport(qual: UseTree, isOnDemand: Boolean,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ImportApi = {
    val res = new Import(qual, isOnDemand)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res
  }
}

object TreeFactories extends TreeFactories
