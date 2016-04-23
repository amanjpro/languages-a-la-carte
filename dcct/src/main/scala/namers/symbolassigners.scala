package ch.usi.inf.l3.sana.dcct.namers

import ch.usi.inf.l3.sana

import sana.ooj
import sana.tiny
import sana.calcj
import sana.primj

import tiny.dsl._

import tiny.ast.{TreeCopiers => _, _}
import calcj.ast.{TreeCopiers => _, _}
import primj.ast.{TreeCopiers => _, _}
import primj.ast.ProgramApi
import tiny.ast.DefTree
import ooj.ast.{ClassDefApi, TemplateApi}
import ooj.symbols.ClassSymbol


@component
trait ClassDefSymbolAssignerComponent extends ooj.namers.ClassDefSymbolAssignerComponent {
  override protected def addDefaultConstructor(clazz: ClassDefApi, sym: ClassSymbol ): TemplateApi =
    // since we do not want a defauly constructor, we just return the tree without modifications
    clazz.body
}


//@component IndexIntSymbolAssignerComponent extends calcj.namers
