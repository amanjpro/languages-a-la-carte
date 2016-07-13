/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
import calcj.typechecker.TyperComponent
import primj.typechecker.TypePromotions
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

  /**
   * Given a type, this method creates an array-type of it.
   *
   * @param tpe the componentType of the array-type to be created
   */
  protected def toArrayType(tpe: Type): Type =
    ArrayType(tpe)


  /**
   * Validates the type of the size of an array creation. This method makes
   * sure that type of array-size is either int, or can be promoted to int.
   *
   * @param size the expression which represents the size
   */
  protected def validateSizeType(size: Option[Expr]): Option[Expr] = {
    for {
      s   <- size
      tpe <- s.tpe
    } yield {
      if(!(tpe <:< IntType)) {
        error(ARRAY_SIZE_NOT_INT, "", "", s.pos)
        s
      } else {
        compiler.typeCheck(s.owner)(
          TypePromotions.castIfNeeded(s, tpe, IntType)
        ).asInstanceOf[Expr]
      }
    }
  }
}


@component
trait UnaryTyperComponent extends primj.typechecker.UnaryTyperComponent {
  /** @see [[arrayj.ast.TreeUtils.isArrayAccessOrVariableAccess]] */
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

  /**
   * Type-checks an array-access expression
   *
   * @param access the array-access expression to be type-checked
   */
  protected def typeArrayAccess(access: ArrayAccessApi): Unit =
    access.array.tpe.map {
      case atpe: ArrayTypeApi          =>
        access.tpe = atpe.componentType
      case _                           =>
        error(NON_ARRAY_ELEMENT_ACCESS, "", "", access.pos)
    }

  /**
   * Validates the type of the index of an array-access. This method makes
   * sure that type of array-index is either int, or can be promoted to int.
   *
   * @param size the expression which represents the array-index
   */
  protected def validateIndexType(index: Expr): Expr =
    index.tpe.map { tpe =>
      if(!(tpe <:< IntType)) {
        error(ARRAY_SIZE_NOT_INT, "", "", index.pos)
        index
      } else {
        compiler.typeCheck(index.owner)(
          TypePromotions.castIfNeeded(index, tpe, IntType)
        ).asInstanceOf[Expr]
      }
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
    if(checkArrayInitializerType(res)) {
      val elements2 = narrowDownOrWidenElemsIfNeeded(elements, init.componentType)
      TreeCopiers.copyArrayInitializer(res)(elements = elements2)
    } else res
  }

  /**
   * Given a type, this method creates an array-type of it.
   *
   * @param tpe the componentType of the array-type to be created
   */
  protected def toArrayType(tpe: Type): Type =
    ArrayType(tpe)

  /**
   * Sets the component type property of the components if needed. It is only needed
   * to set the component types, if the component itself is an array initialization
   * expression.
   *
   * @param init the array initialization that we may set component-type for its
   *             components
   */
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

  /**
   * Narrows down, or widens elements of an array initialization when needed.
   *
   * @param elements the elements of an array initialization
   * @param ctpe the expected type of the components of an array
   */
  protected def narrowDownOrWidenElemsIfNeeded(elements: List[Expr],
      ctpe: Option[() =>Type]): List[Expr] = {
    ctpe match {
      case Some(bt)      =>
        val ctpe = bt()
        elements.flatMap { elem =>
          elem.tpe.map { tpe =>
            if(ctpe =:= tpe) {
              elem
            } else if(ctpe >:> tpe) {
              typed(widenIfNeeded(elem, Some(ctpe))).asInstanceOf[Expr]
            } else {
              elem match {
                case lit: LiteralApi        =>
                  typed(narrowDown(lit, ctpe)).asInstanceOf[Expr]
                case e                      => elem
              }
            }
          }
        }
      case _             =>
        elements
    }
  }

  /** @see [[sana.primj.typechecker.TypePromotions.widenIfNeeded]] */
  protected def widenIfNeeded(expr: Expr, tpe: Option[Type]): Expr =
    TypePromotions.widenIfNeeded(expr, tpe)

  /**
   * Checks the types of the elements of array initialization expression
   *
   * @param init the array initialization expression to be checked
   */
  protected def checkArrayInitializerType(init: ArrayInitializerApi): Boolean = {
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
    !hasErrors
  }


  /** @see [[sana.primj.typechecker.TypePromotions.isNarrawableTo]] */
  protected def isNarrawableTo(expr: Tree, tpe: Type): Boolean =
    TypePromotions.isNarrawableTo(expr, tpe)

  /** @see [[arrayj.ast.TreeUtils.narrowDown]] */
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
    val rhs2 = typed(widenIfNeeded(valdef.rhs, valdef.tpe)).asInstanceOf[Expr]
    TreeCopiers.copyValDef(res)(rhs = rhs2)
    res
  }

  /**
   * Returns the type of the component of an array
   *
   * @param symbol the symbol of the array
   */
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
