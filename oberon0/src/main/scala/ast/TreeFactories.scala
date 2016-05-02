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


trait TreeFactories extends arrayj.ast.TreeFactories {

  def mkModuleDef(name: Name, declarations: List[DefTree],
    block: Option[BlockApi], pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): ModuleDefApi = {

    val res = new ModuleDef(name, declarations, block)
    pos.foreach(res.pos = _)
    symbol.foreach(res.symbol = _)
    owner.foreach(res.owner = _)
    res
  }


  def mkTypeDef(name: Name, tpt: UseTree,
    pos: Option[Position] = None, symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): TypeDefApi = {

    val res = new TypeDef(name, tpt)
    pos.foreach(res.pos = _)
    symbol.foreach { s =>
      s.tpe.foreach(res.tpe = _)
      res.symbol = s
    }
    owner.foreach(res.owner = _)
    res
  }
}

object TreeFactories extends TreeFactories
