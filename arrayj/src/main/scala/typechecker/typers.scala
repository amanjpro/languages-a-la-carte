package ch.usi.inf.l3.sana.arrayj.typechecker


import ch.usi.inf.l3.sana
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.types._
import primj.ast.TreeExtractors._
import primj.ast.ValDefApi
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
import arrayj.ast.Implicits._




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
    val size  = {
      val temp = creation.size.map(size => typed(size).asInstanceOf[Expr])
      validateSizeType(temp)
    }

    val res =
      TreeCopiers.copyArrayCreation(creation)(array = array, size = size)
    array.tpe.foreach(res.tpe = _)
    res
  }


  protected def validateSizeType(size: Option[Expr]): Option[Expr] = {
    for {
      s   <- size
      tpe <- s.tpe
    } yield {
      if(!(tpe <:< IntType)) {
        error(ARRAY_SIZE_NOT_INT, "", "", s.pos)
        s
      } else TypePromotions.castIfNeeded(s, tpe, IntType)
    }
  }
}


@component
trait ArrayAccessTyperComponent extends TyperComponent {
  (access: ArrayAccessApi) => {
    val array = typed(access.array).asInstanceOf[Expr]
    val index = {
      val temp = typed(access.index).asInstanceOf[Expr]
      validateIndexType(temp)
    }
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

  protected def validateIndexType(index: Expr): Expr =
    index.tpe.map { tpe =>
      if(!(tpe <:< IntType)) {
        error(ARRAY_SIZE_NOT_INT, "", "", index.pos)
        index
      } else TypePromotions.castIfNeeded(index, tpe, IntType)
    }.getOrElse(index)
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
    val elements = init.elements.map { elem =>
      (init.componentType, elem) match {
        case (Some(ArrayType(ArrayType(t))), elem: ArrayInitializerApi)    =>
          elem.componentType = t
        case _                                                             =>
      }
      typed(elem).asInstanceOf[Expr]
    }
    val res      = TreeCopiers.copyArrayInitizalizer(init)(elements = elements)
    checkArrayInitializerType(res)
    res
  }

  protected def checkArrayInitializerType(init: ArrayInitializerApi): Unit = {
    val hasErrors = init.elements.foldLeft(false)((z, y) => {
      val r = for {
        etpe <- y.tpe
        ctpe <- init.componentType
      } yield {
        if(etpe <:< ctpe) z
        else {
          error(TYPE_MISMATCH, ctpe.toString, etpe.toString, y.pos)
          true
        }
      }
      r.getOrElse(false)
    })
    if(!hasErrors) {
      init.componentType.foreach(init.tpe = _)
    }
  }
}


@component
trait ValDefTyperComponent extends primj.typechecker.ValDefTyperComponent {
  (valdef: ValDefApi)          => {
    val tpt    = typed(valdef.tpt).asInstanceOf[UseTree]
    (tpt.tpe, valdef.rhs) match {
      case (Some(ArrayType(bt)), rhs: ArrayInitializerApi) =>
        rhs.componentType = bt
      case _                                               =>
        ()
    }
    super.apply(valdef)
  }
}
