package ch.usi.inf.l3.sana.dcct.ast

import ch.usi.inf.l3.sana
import sana.primj._
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.primj.ast.Implicits._
import sana.tiny.modifiers.Flags

import sana.tiny.ast._
import sana.calcj.ast._
import sana.primj.ast._


trait TreeFactories extends sana.ooj.ast.TreeFactories {
  
  def mkArrayDef(name: Name, indices: List[UseTree], 
      properties: List[Expr], symbol: Option[Symbol] = None) : ArrayDefApi = {
    val res = new ArrayDef(name, indices, properties)
    symbol.foreach ( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    res
  }
 
  def mkForEach(inits: List[Tree], allOrEntries: ApplyApi,
   cond: Expr, body: Expr, symbol: Option[Symbol] = None): ForEach = {
    val res = new ForEach(inits, allOrEntries, cond, body)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    res
  }     
}

object TreeFactories extends TreeFactories
