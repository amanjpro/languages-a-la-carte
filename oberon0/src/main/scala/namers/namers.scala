package ch.usi.inf.l3.sana.oberon0.namers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import sana.oberon0

import tiny.dsl._
import oberon0.ast._
import tiny.ast.{DefTree, UseTree}
import tiny.names.Name
import tiny.source.Position
import tiny.symbols.{Symbol, TypeSymbol}
import ooj.symbols.ClassSymbol
import ooj.ast.{ClassDefApi, TemplateApi}
import tiny.errors.ErrorReporting.{error, warning}
import oberon0.errors.ErrorCodes._
import primj.ast.BlockApi
import oberon0.ast.Implicits._
import oberon0.symbols._
import primj.namers.NamerComponent
import oberon0.symbols.SymbolUtils

@component
trait ModuleDefNamerComponent extends NamerComponent {
  (module: ModuleDefApi) => {
    val declarations = module.declarations.map(name(_).asInstanceOf[DefTree])
    val block        = module.block.map(name(_).asInstanceOf[BlockApi])
    TreeCopiers.copyModuleDef(module)(declarations = declarations, block = block)
  }
}




@component
trait TypeDefNamerComponent extends NamerComponent {
  (tdef: TypeDefApi) => {
    val tpt = name(tdef.tpt).asInstanceOf[UseTree]
    tdef.symbol.foreach {
      case sym: TypeDefSymbol => sym.typeSymbol = tpt.symbol
      case _                  => ()
    }
    TreeCopiers.copyTypeDef(tdef)(tpt = tpt)
  }
}

@component
trait ClassDefNamerComponent extends ooj.namers.ClassDefNamerComponent {

  override protected def addObjectParentIfNeeded(
    clazz: ClassDefApi): List[UseTree] = Nil

  override protected def packageName(symbol: Option[Symbol]): String = {
    val mdl = SymbolUtils.enclosingModule(symbol)
    mdl.map(_.name.asString).getOrElse("")
  }
}
