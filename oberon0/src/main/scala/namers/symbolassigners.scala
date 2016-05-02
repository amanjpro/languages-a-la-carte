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
import primj.namers.SymbolAssignerComponent




@component
trait ModuleDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (module: ModuleDefApi) => {
    val owner = module.owner
    checkDoubleDef(module, owner)
    val symbol = ModuleSymbol(module.name, owner)
    module.symbol = symbol
    module.declarations.foreach(_.owner = symbol)
    module.block.foreach(_.owner = symbol)
    val declarations = module.declarations.map(assign(_).asInstanceOf[DefTree])
    val block = module.block.map(assign(_).asInstanceOf[BlockApi])
    TreeCopiers.copyModuleDef(module)(declarations = declarations, block = block)
  }


  def checkDoubleDef(module: ModuleDefApi, owner: Option[Symbol]): Unit = {
    owner.foreach { owner =>
      owner.directlyDefinesName(module.name,
        _.isInstanceOf[ModuleSymbol]) match {
        case true         =>
          error(MODULE_ALREADY_DEFINED, "", "", module.pos)
        case _            =>
          ()
      }
    }
  }
}


@component
trait ClassDefSymbolAssignerComponent
    extends ooj.namers.ClassDefSymbolAssignerComponent {
  override protected def classDoubleDefCheck(owner: Option[Symbol],
    name: Name, pos: Option[Position]): Unit = owner match {
    case Some(owner) if owner.directlyDefinesName(name,
                               _.isInstanceOf[TypeSymbol])      =>
      error(TYPE_ALREADY_DEFINED,
          "", "", pos)
    case _                                                      =>
      ()
  }

  override protected def addDefaultConstructor(clazz: ClassDefApi,
    sym: ClassSymbol): TemplateApi = {
    clazz.body
  }
}


@component
trait TypeDefSymbolAssigner
    extends SymbolAssignerComponent {

  (tdef: TypeDefApi) => {
    val owner = tdef.owner
    doubleDefCheck(owner, tdef.name, tdef.pos)
    val symbol = TypeDefSymbol(tdef.name, None, owner)
    tdef.symbol = symbol
    owner.foreach(tdef.tpt.owner = _)
    val tpt    = assign(tdef.tpt).asInstanceOf[UseTree]
    TreeCopiers.copyTypeDef(tdef)(tpt = tpt)
  }

  protected def doubleDefCheck(owner: Option[Symbol],
    name: Name, pos: Option[Position]): Unit = owner match {
    case Some(owner) if owner.directlyDefinesName(name,
                               _.isInstanceOf[TypeSymbol])      =>
      error(TYPE_ALREADY_DEFINED,
          "", "", pos)
    case _                                                      =>
      ()
  }
}
