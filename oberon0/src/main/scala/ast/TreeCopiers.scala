package ch.usi.inf.l3.sana.oberon0.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.arrayj

import tiny.ast.{Tree, DefTree, TypeTree, TermTree, UseTree}
import tiny.ast.Implicits._
import primj.ast.{BlockApi}
import tiny.names.Name
import tiny.symbols.Symbol
import tiny.source.Position


trait TreeCopiers extends arrayj.ast.TreeCopiers {

  def copyModuleDef(template: ModuleDefApi)(name: Name = template.name,
    declarations: List[DefTree] = template.declarations,
    block: Option[BlockApi] = template.block): ModuleDefApi = {
    val res = TreeFactories.mkModuleDef(name, declarations, block)
    copyProperties(template, res)
    res
  }


  def copyTypeDef(template: TypeDefApi)(name: Name = template.name,
    tpt: UseTree = template.tpt): TypeDefApi = {

    val res = TreeFactories.mkTypeDef(name, tpt)
    copyProperties(template, res)
    res
  }
}

object TreeCopiers extends TreeCopiers
