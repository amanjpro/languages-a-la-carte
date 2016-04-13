package ch.usi.inf.l3.sana.arrooj.typechecker


import ch.usi.inf.l3.sana
import sana.ooj
import sana.arrooj
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.dsl._
import tiny.ast.{UseTree, Expr, Tree}
import tiny.types.Type
import tiny.symbols.Symbol
import tiny.errors.ErrorReporting.{error, warning}
import calcj.typechecker.TyperComponent
import arrayj.ast.{TreeUtils => _,
                   TreeCopiers => _, _}
import arrooj.ast._
import primj.ast.ValDefApi
import arrooj.ast.Implicits._
import arrooj.symbols.{ArraySymbol, SymbolUtils}
import arrooj.types.{ArrayType, TypeUtils}
import arrooj.errors.ErrorCodes._

@component
trait ArrayAccessTyperComponent
  extends arrayj.typechecker.ArrayAccessTyperComponent {

  override protected def typeArrayAccess(access: ArrayAccessApi): Unit = {
    access.array.tpe.map {
      case atpe: ArrayType             =>
        access.tpe = atpe.componentType
      case _                           =>
        error(NON_ARRAY_ELEMENT_ACCESS, "", "", access.pos)
    }
    access.array.symbol.foreach {
      case sym: ArraySymbol =>
        access.symbol = sym.componentSymbol
      case _                =>
        ()
    }
  }
}


@component
trait ArrayTypeUseTyperComponent
  extends TyperComponent {
  (tuse: ArrayTypeUseApi) => {
    val tpt = typed(tuse.tpt).asInstanceOf[UseTree]
    val res =
      TreeCopiers.copyArrayTypeUse(tuse)(tpt = tpt)
    tpt.tpe.foreach { tpe =>
      res.tpe = TypeUtils.mkArrayType(tpe)
    }
    tpt.symbol.foreach { sym =>
      res.symbol = SymbolUtils.mkArraySymbol(sym)
    }
    res
  }
}

@component
trait ArrayInitializerTyperComponent
  extends arrayj.typechecker.ArrayInitializerTyperComponent {

  override protected def setComponentTypesIfNeeded(
    init: ArrayInitializerApi): List[Expr]= init.elements.map { elem =>
      (init.componentType, elem) match {
        case (Some(bt), elem: ArrayInitializerApi)    =>
          bt() match {
            case ArrayType(ArrayType(t)) =>
              elem.componentType = () => t
            case _                       =>
          }
        case _                                                             =>
          ()
      }
      typed(elem).asInstanceOf[Expr]
    }


  override protected def toArrayType(tpe: Type): Type =
    TypeUtils.mkArrayType(tpe)
}

@component
trait ValDefTyperComponent extends ooj.typechecker.ValDefTyperComponent {
  (valdef: ValDefApi)          => {
    val res = valdef.rhs match {
      case rhs: ArrayInitializerApi =>
        getComponentType(valdef.symbol).foreach { bt =>
          rhs.componentType = bt
        }
      case _                       =>
        ()
    }
    super.apply(valdef)
  }


  protected def getComponentType(
      symbol: Option[Symbol]): Option[() => Type] =
    symbol.flatMap(_.tpe.flatMap {
      case tpe: ArrayType        => Some(() => tpe.componentType)
      case _                     => None
    })

}

@component
trait SelectTyperComponent extends ooj.typechecker.SelectTyperComponent {
  override protected def isTypeUse(tree: Tree): Boolean = tree match {
    case t: UseTree => TreeUtils.isTypeUse(t)
    case _          => false
  }
}

trait ArrayCreationTyperComponent
  extends arrayj.typechecker.ArrayCreationTyperComponent {

  override protected def toArrayType(tpe: Type): Type =
    TypeUtils.mkArrayType(tpe)
}


@component
trait AssignTyperComponent extends ooj.typechecker.AssignTyperComponent {
  override protected def checkVariableLHS(lhs: Tree): Unit = {
    if(!TreeUtils.isArrayAccessOrVariableAccess(lhs))
      error(ASSIGNING_NOT_TO_VARIABLE,
        lhs.toString, lhs.toString, lhs.pos)
  }
}
