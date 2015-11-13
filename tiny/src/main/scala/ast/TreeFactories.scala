package ch.usi.inf.l3.sana.tiny.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import Implicits._



trait TreeFactories {

  def mkIdent(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            owner: Option[Symbol] = None): IdentApi = {
    val res = Ident(name)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    symbol.foreach(sym => {
      res.symbol = sym
      sym.tpe.foreach(res.tpe = _)
    })
    res
  }

  def mkTypeUse(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            owner: Option[Symbol] = None): TypeUseApi = {
    val res = TypeUse(name)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    symbol.foreach(sym => {
      res.symbol = sym
      sym.tpe.foreach(res.tpe = _)
    })
    res
  }
}


object TreeFactories extends TreeFactories

