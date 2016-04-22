package ch.usi.inf.l3.sana.arrooj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.brokenj
import sana.ooj
import sana.arrayj

import tiny.symbols.Symbol
import tiny.types.Type
import tiny.ast.{Tree, Expr, UseTree}
import tiny.source.Position
import arrayj.ast.{TreeFactories => ATreeFactories, _}

trait TreeFactories extends ooj.ast.TreeFactories {
  def mkArrayInitizalizer(elements: List[Expr],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayInitializerApi =
    ATreeFactories.mkArrayInitizalizer(elements, pos, symbol, owner, tpe)


  def mkArrayAccess(array: Expr, index: Expr,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayAccessApi =
    ATreeFactories.mkArrayAccess(array, index, pos, symbol, owner, tpe)

  def mkArrayTypeUse(tpt: UseTree,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayTypeUseApi =
    ATreeFactories.mkArrayTypeUse(tpt, pos, symbol, owner, tpe)

  def mkArrayCreation(array: Expr,
    size: Option[Expr],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayCreationApi =
    ATreeFactories.mkArrayCreation(array, size, pos, symbol, owner, tpe)
}

object TreeFactories extends TreeFactories

