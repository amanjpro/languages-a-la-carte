package ch.usi.inf.l3.sana.brokenj.typechecker


import ch.usi.inf.l3.sana
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.types._
import primj.ast.Implicits._
import primj.ast.TreeExtractors._
import tiny.types.TypeUtils._
import tiny.symbols.{TypeSymbol, TermSymbol}
import tiny.source.Position
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.{TyperComponent, TypePromotions}
import calcj.types._
import primj.symbols._
import primj.types._
import primj.modifiers.Ops._
import arrayj.ast._
import arrayj.errors.ErrorCodes._
import arrayj.types._




/*
ArrayCreation: DONE
ArrayAccess: DONE
ArrayTypeUse: DONE
ArrayInitializer:
*/

@component
trait ArrayCreationTyperComponent extends TyperComponent {
  (creation: ArrayCreationApi) => {
    val array = typed(creation.array).asInstanceOf[Expr]
    val size  = creation.size.map(size => typed(size).asInstanceOf[Expr])

    checkSizeType(size)

    val res =
      TreeCopiers.copyArrayCreation(creation)(array = array, size = size)
    array.tpe.foreach(res.tpe = _)
    res
  }


  protected def checkSizeType(size: Option[Expr]): Unit =
    for {
      s   <- size
      tpe <- s.tpe if !(tpe <:< IntType)
    } {
      error(ARRAY_SIZE_NOT_INT, "", "", s.pos)
    }
}


@component
trait ArrayAccessTyperComponent extends TyperComponent {
  (access: ArrayAccessApi) => {
    val array = typed(access.array).asInstanceOf[Expr]
    val index = typed(access.index).asInstanceOf[Expr]
    checkIndexType(index)
    val res =
      TreeCopiers.copyArrayAccess(access)(array = array, index = index)
    array.tpe.foreach {
      case atpe: ArrayTypeApi       =>
        res.tpe = atpe.componentType
      case _                        =>
        error(NON_ARRAY_ELEMENT_ACCESS, "", "", access.pos)
    }
    res
  }

  protected def checkIndexType(index: Expr): Unit =
    index.tpe foreach { tpe =>
      if(!(tpe <:< IntType))
        error(ARRAY_SIZE_NOT_INT, "", "", index.pos)
    }
}


@component
trait ArrayTypeUseTyperComponent extends TyperComponent {
  (tuse: ArrayTypeUseApi) => {
    val tpt = typed(tuse.tpt).asInstanceOf[UseTree]
    val res =
      TreeCopiers.copyArrayTypeUse(tuse)(tpt = tpt)
    tpt.tpe.foreach(tpe => res.tpe = ArrayType(tpe))
    res
  }
}

@component
trait ArrayInitializerTyperComponent extends TyperComponent {
  (init: ArrayInitializerApi) => {
    val elements = init.elements.map (elem => typed(elem).asInstanceOf[Expr])
    val tpe      = unifyArrayElements(elements)
    val res      = TreeCopiers.copyArrayInitizalizer(init)(elements = elements)
    res.tpe      = ArrayType(tpe)
    res
  }

  protected def unifyArrayElements(elements: List[Expr]): Type = {
    // TODO: implement me
    ???
  }
}
