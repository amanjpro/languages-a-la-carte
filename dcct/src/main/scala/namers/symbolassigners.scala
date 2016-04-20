package ch.usi.inf.l3.sana.dcct.namers

import ch.usi.inf.l3.sana

import sana.ooj
import sana.tiny
import sana.dsl

import dsl._
import ooj.ast.{ClassDefApi, TemplateApi}
import ooj.symbols.ClassSymbol

@component
trait ClassDefSymbolAssignerComponent extends ooj.namers.ClassDefSymbolAssignerComponent {
  override protected def addDefaultConstructor(clazz: ClassDefApi, sym: ClassSymbol ): TemplateApi =
    clazz.body
}
