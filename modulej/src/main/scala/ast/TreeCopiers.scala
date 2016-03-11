package ch.usi.inf.l3.sana.modulej.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.ast.Implicits._
import sana.tiny.ast.UseTree



trait TreeCopiers extends sana.ppj.ast.TreeCopiers {
  def copyImport(template: ImportApi)(
      qual: UseTree = template.qual,
      isOnDemand: Boolean = template.isOnDemand,
      pos: Option[Position] = template.pos,
      owner: Option[Symbol]  = template.owner): ImportApi = {
    val res = TreeFactories.mkImport(qual, isOnDemand)
    copyProperties(template, res)
    res
  }
}

object TreeCopiers extends TreeCopiers
