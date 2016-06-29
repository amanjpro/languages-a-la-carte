package ch.usi.inf.l3.sana.dcct.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.primj.ast.Implicits._
import sana.tiny.modifiers.Flags
import sana.tiny.ast._
import sana.calcj.ast._
import sana.primj.ast._

import sana.ooj.ast._
import sana.dcct.ast._

trait TreeCopiers extends sana.ooj.ast.TreeCopiers {
  
  def copyArrayDef(template: ArrayDefApi)(name: Name = template.name, 
      indices: List[ValDefApi] = template.indices,
      properties: List[ValDefApi] = template.properties): ArrayDefApi =  {
    
    val res = sana.dcct.ast.
        TreeFactories.mkArrayDef(name, indices, properties)
        
    copyProperties(template, res)
    res
  }
  
  def copyForEach(template: ForEach)(entityVar: ValDefApi,
    whereExpr: Expr, body: BlockApi): ForEachApi = {
    
    val res = sana.dcct.ast.
      TreeFactories.mkForEach(entityVar, whereExpr, body)
      
    copyProperties(template, res)
    res
  }
}

object TreeCopiers extends TreeCopiers
