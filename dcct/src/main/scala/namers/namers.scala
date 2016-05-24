package ch.usi.inf.l3.sana.dcct.namers
import ch.usi.inf.l3.sana
import sana.ooj
import sana.primj
import sana.tiny
import sana.calcj
import sana.dcct

import tiny.core.TransformationComponent
import tiny.dsl._
import tiny.ast.{TreeCopiers => _, TreeFactories => _, _}
import tiny.names.Name
import tiny.symbols._
import tiny.errors.ErrorReporting.{error,warning}
import calcj.ast.{TreeCopiers => _, TreeFactories => _, _}
import calcj.ast.operators.{Inc, Dec}
import primj.namers.NamerComponent
import primj.symbols.{SymbolUtils => _, _}
import primj.errors.ErrorCodes._
import primj.ast.{ApplyApi, BlockApi}
import ooj.ast._
import ooj.ast.TreeExtractors._
import ooj.types.ClassType
import ooj.names.StdNames
import ooj.modifiers.Ops._
import ooj.symbols.{SymbolUtils, ClassSymbol, PackageSymbol}
import ooj.ast.Implicits._
import dcct.ast._

@component 
trait ClassDefNamerComponent extends ooj.namers.ClassDefNamerComponent {
  
  override def addObjectParentIfNeeded (clazz: ClassDefApi): List[UseTree] = {
    Nil
  }
}
@component
trait ArrayDefNamerComponent extends NamerComponent {
  (array: ArrayDefApi)  => array
}

@component
trait ForeachNamerComponent extends NamerComponent {
  (foreach: ForEachApi)  => foreach
}


