package ch.usi.inf.l3.sana.arrayj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.brokenj

import tiny.symbols.Symbol
import tiny.types.Type
import tiny.ast.{Tree, Expr, UseTree}
import tiny.source.Position
import primj.ast.Implicits._

trait TreeFactories extends brokenj.ast.TreeFactories {

  def mkArrayInitizalizer(elements: List[Expr],
      pos: Option[Position] = None,
      symbol: Option[Symbol] = None,
      owner: Option[Symbol] = None,
      tpe: Option[Type] = None): ArrayInitializerApi = {
    val res = new ArrayInitializer(elements)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    symbol.foreach(res.symbol = _)
    tpe.foreach(res.tpe = _)
    res
  }


  def mkArrayAccess(array: Expr, index: Expr,
      pos: Option[Position] = None,
      symbol: Option[Symbol] = None,
      owner: Option[Symbol] = None,
      tpe: Option[Type] = None): ArrayAccessApi = {
    val res = new ArrayAccess(array, index)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    symbol.foreach(res.symbol = _)
    tpe.foreach(res.tpe = _)
    res
  }

  def mkArrayTypeUse(tpt: UseTree,
      pos: Option[Position] = None,
      symbol: Option[Symbol] = None,
      owner: Option[Symbol] = None,
      tpe: Option[Type] = None): ArrayTypeUseApi = {
    val res = new ArrayTypeUse(tpt)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    symbol.foreach(res.symbol = _)
    tpe.foreach(res.tpe = _)
    res
  }

  def mkArrayCreation(array: Expr,
      size: Option[Expr],
      pos: Option[Position] = None,
      symbol: Option[Symbol] = None,
      owner: Option[Symbol] = None,
      tpe: Option[Type] = None): ArrayCreationApi = {
    val res = new ArrayCreation(array, size)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    symbol.foreach(res.symbol = _)
    tpe.foreach(res.tpe = _)
    res
  }
}


object TreeFactories extends TreeFactories
