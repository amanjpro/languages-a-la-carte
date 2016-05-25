package ch.usi.inf.l3.sana.dcct.namers

import ch.usi.inf.l3.sana

import sana.ooj
import sana.tiny
import sana.calcj
import sana.primj
import sana.dcct
import tiny.dsl._

import tiny.ast.{TreeCopiers => _, _}
import calcj.ast.{TreeCopiers => _, _}
import primj.ast.{TreeCopiers => _, _}
import primj.ast.ProgramApi
import tiny.ast.DefTree
import ooj.ast.{ClassDefApi, TemplateApi}
import ooj.symbols.ClassSymbol
import ooj.ast.Implicits._
import dcct.symbols._
import dcct.ast._
import primj.namers.SymbolAssignerComponent
import primj.symbols.ScopeSymbol


@component
trait ClassDefSymbolAssignerComponent extends ooj.namers.ClassDefSymbolAssignerComponent {
  
  override protected def addDefaultConstructor(clazz: ClassDefApi, 
   sym: ClassSymbol ): TemplateApi = {
    clazz.body.owner = sym
    assign(clazz.body).asInstanceOf[TemplateApi]
  }
}

@component
trait ArrayDefSymbolAssignerComponent extends SymbolAssignerComponent {
  // TODO why do I need the arraySymbol anyway???
  (array: ArrayDefApi) => { 
    array.indices.map { x =>
        x.owner = ArraySymbol(array.name)
        assign(x).asInstanceOf[ValDefApi]
    }
    array
  }
}

@component 
trait ForeachSymbolAssignerComponent extends SymbolAssignerComponent {
  (foreach: ForEachApi) => {
    val owner = foreach.owner
    val symbol = ScopeSymbol(owner)
    foreach.entityVar.owner = symbol
    foreach.body.owner = symbol
    foreach.symbol = symbol
    foreach
  }
}



