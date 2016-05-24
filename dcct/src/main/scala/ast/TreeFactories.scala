package ch.usi.inf.l3.sana.dcct.ast

import ch.usi.inf.l3.sana
import sana.primj._
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.primj.ast.Implicits._
import sana.primj.types.MethodType
import sana.tiny.modifiers.Flags

import sana.tiny.ast._
import sana.calcj.ast._
import sana.primj.ast._


trait TreeFactories extends sana.ooj.ast.TreeFactories {
  // TODO Copied from primj factories. Is there a nicer way to do it?
  def mkActionDef(ret: UseTree,
    name: Name, params: List[ValDefApi],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): MethodDefApi = {
    val res = new MethodDef(ret, name, params, body)
    pos.foreach(res.pos = _)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    val tys = params.flatMap(_.tpe)
    ret.tpe.foreach(t => res.tpe = MethodType(t, tys))
    res
  }

  
  def mkArrayDef(name: Name, indices: List[ValDefApi], 
      properties: List[ValDefApi], symbol: Option[Symbol] = None) : ArrayDefApi = {
    val res = new ArrayDef(name, indices, properties)
    symbol.foreach ( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    res
  }
 
  def mkForEach(entityVar: ValDefApi, whereExpr: Expr, body: BlockApi,
    symbol: Option[Symbol] = None): ForEach = {
    val res = new ForEach(entityVar, whereExpr, body)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    res
  }     
}

object TreeFactories extends TreeFactories
