package ch.usi.inf.l3.sana.arrayj.typechecker


import ch.usi.inf.l3.sana
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.types._
import primj.ast.TreeExtractors._
import primj.ast.ValDefApi
import tiny.types.TypeUtils._
import tiny.symbols.{Symbol, TypeSymbol, TermSymbol}
import tiny.source.Position
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.{TyperComponent, TypePromotions}
import calcj.types._
import calcj.ast.LiteralApi
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
    array.tpe.foreach(tpe => res.tpe = toArrayType(tpe))
    res
  }

  protected def toArrayType(tpe: Type): Type =
    ArrayType(tpe)


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
trait UnaryTyperComponent extends primj.typechecker.UnaryTyperComponent {
  override protected def isVariable(tree: Tree): Boolean =
    TreeUtils.isArrayAccessOrVariableAccess(tree)
}

@component
trait ArrayAccessTyperComponent extends TyperComponent {
  (access: ArrayAccessApi) => {
    val array = typed(access.array).asInstanceOf[Expr]
    val index = {
      val temp = typed(access.index).asInstanceOf[Expr]
      validateIndexType(temp)
    }
    val res = TreeCopiers.copyArrayAccess(access)(
      array = array, index = index).asInstanceOf[ArrayAccessApi]
    typeArrayAccess(res)
    res
  }

  protected def typeArrayAccess(access: ArrayAccessApi): Unit =
    access.array.tpe.map {
      case atpe: ArrayTypeApi          =>
        access.tpe = atpe.componentType
      case _                           =>
        error(NON_ARRAY_ELEMENT_ACCESS, "", "", access.pos)
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
    val elements = setComponentTypesIfNeeded(init)
    val res      = TreeCopiers.copyArrayInitializer(init)(elements = elements)
    checkArrayInitializerType(res)
    val elements2 = narrawDownElemsIfNeeded(elements, init.componentType)
    TreeCopiers.copyArrayInitializer(res)(elements = elements2)
  }

  protected def toArrayType(tpe: Type): Type =
    ArrayType(tpe)

  protected def setComponentTypesIfNeeded(
    init: ArrayInitializerApi): List[Expr]= init.elements.map { elem =>
      (init.componentType, elem) match {
        case (Some(bt), elem: ArrayInitializerApi)    =>
          bt() match {
            case ArrayType(t) =>
              elem.componentType = () => t
            case _            =>
          }
        case _                                                             =>
          ()
      }
      typed(elem).asInstanceOf[Expr]
    }

  protected def narrawDownElemsIfNeeded(elements: List[Expr],
      ctpe: Option[() =>Type]): List[Expr] = {
    ctpe match {
      case Some(bt)      =>
        val ctpe = bt()
            if(ctpe =:= IntType) {
          for {
            elem <- elements
          } yield {
            elem match {
              case lit: LiteralApi        => narrowDown(lit, ctpe)
              case e                      => e
            }
          }
        } else elements
      case _             =>
        elements
    }
  }
  protected def checkArrayInitializerType(init: ArrayInitializerApi): Unit = {
    val hasErrors = init.elements.foldLeft(false)((z, y) => {
      val r = for {
        etpe <- y.tpe
        ctpe <- init.componentType
      } yield {
        val ctpe2 = ctpe()
        if(etpe <:< ctpe2) z
        else if(isNarrawableTo(y, ctpe2)) z
        else {
          error(TYPE_MISMATCH, ctpe.toString, etpe.toString, y.pos)
          true
        }
      }
      r.getOrElse(false)
    })
    init.componentType.foreach { tpe =>
      init.tpe = toArrayType(tpe())
    }
  }


  protected def isNarrawableTo(expr: Tree, tpe: Type): Boolean =
    TypePromotions.isNarrawableTo(expr, tpe)

  // TODO: I need to do this
  protected def narrowDown(lit: LiteralApi, tpe: Type): LiteralApi =
    TreeUtils.narrowDown(lit, tpe)
}


@component
trait ValDefTyperComponent extends primj.typechecker.ValDefTyperComponent {
  (valdef: ValDefApi)          => {
    if(!valdef.mods.isField) {
      checkDoubleDef(valdef.owner, valdef.name, valdef.pos)
      valdef.owner.foreach(sym => {
        valdef.symbol.foreach(sym.declare(_))
      })
    }
    val tpt    = typed(valdef.tpt).asInstanceOf[UseTree]
    valdef.symbol.foreach(sym => {
      sym.tpe.foreach(valdef.tpe = _)
      sym match {
        case vs: VariableSymbol    =>
          vs.typeSymbol = tpt.symbol
        case _                     =>
          ()
      }
    })
    valdef.rhs match {
      case rhs: ArrayInitializerApi =>
        getComponentType(tpt.symbol).foreach { bt =>
          rhs.componentType = bt
        }
      case _                       =>
        ()
    }
    val rhs    = typed(valdef.rhs).asInstanceOf[Expr]
    val ttpe   = tpt.tpe.getOrElse(ErrorType)
    valdef.tpe = ttpe
    val res = TreeCopiers.copyValDef(valdef)(tpt = tpt, rhs = rhs)
    checkValDef(res)
    res
  }

  protected def getComponentType(
      symbol: Option[Symbol]): Option[() => Type] =
    symbol.flatMap(_.tpe.flatMap {
      case tpe: ArrayType        => Some(() => tpe.componentType)
      case _                     => None
    })
}


@component
trait AssignTyperComponent extends primj.typechecker.AssignTyperComponent {
  override protected def checkVariableLHS(lhs: Tree): Unit = {
    if(!TreeUtils.isArrayAccessOrVariableAccess(lhs))
      error(ASSIGNING_NOT_TO_VARIABLE,
        lhs.toString, lhs.toString, lhs.pos)
  }
}
