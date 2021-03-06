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
import tiny.ast.{UseTree, Expr, Tree, NoTree}
import tiny.types.Type
import tiny.symbols.Symbol
import tiny.errors.ErrorReporting.{error, warning}
import calcj.typechecker.TyperComponent
import arrayj.ast.{TreeUtils => _,
                   TreeCopiers => _, _}
import arrooj.ast._
import ooj.modifiers.Ops._
import primj.ast.ValDefApi
import primj.symbols.VariableSymbol
import ooj.ast.SelectApi
import arrooj.ast.Implicits._
import arrooj.ast.TreeExtractors._
import arrooj.symbols.{ArraySymbol, SymbolUtils}
import arrooj.types.{ArrayType, TypeUtils}
import arrooj.errors.ErrorCodes._

@component
trait ArrayAccessTyperComponent
  extends arrayj.typechecker.ArrayAccessTyperComponent {

  /** @see {{{arrayj.typechecker.ArrayAccessTyperComponent.typeArrayAccess}}} */
  override protected def typeArrayAccess(access: ArrayAccessApi): Unit = {
    access.array.tpe.map {
      case atpe: ArrayType             =>
        access.tpe = atpe.componentType
      case _                           =>
        error(NON_ARRAY_ELEMENT_ACCESS, "", "", access.pos)
    }
    access.array.symbol.foreach {
      case sym: ArraySymbol  =>
        access.symbol = sym.componentSymbol
      case v: VariableSymbol =>
        v.typeSymbol.foreach {
          case sym: ArraySymbol  =>
            access.symbol = sym.componentSymbol
          case s                 =>
            ()
        }
      case s                 =>
        ()
    }
  }
}

@component
trait UnaryTyperComponent extends primj.typechecker.UnaryTyperComponent {
  /** @see {{{TreeUtils.isArrayAccessOrVariableAccess}}} */
  override protected def isVariable(tree: Tree): Boolean =
    TreeUtils.isArrayAccessOrVariableAccess(tree)
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

  /** @see {{{arrayj.typechecker.ArrayInitializerTyperComponent}}} */
  override protected def setComponentTypesIfNeeded(
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


  /** @see {{{TypeUtils.mkArrayType}}} */
  override protected def toArrayType(tpe: Type): Type =
    TypeUtils.mkArrayType(tpe)
}

@component
trait ValDefTyperComponent extends ooj.typechecker.ValDefTyperComponent {

  /** @see {{{ooj.typechecker.ValDefTyperComponent.typeRhs}}} */
  protected override def typeRhs(valdef: ValDefApi): Expr = {
    if(valdef.mods.isField &&
                    (valdef.mods.isStatic || !valdef.mods.isFinal) &&
                    valdef.rhs == NoTree) {
      if(valdef.mods.isFinal)
        valdef.hasDefaultInit = true
      val dflt = getDefaultFieldValue(valdef.tpt.tpe)
      valdef.owner.foreach(dflt.owner = _)
      typed(dflt).asInstanceOf[Expr]
    } else {
      valdef.rhs match {
        case rhs: ArrayInitializerApi =>
          getComponentType(valdef.symbol).foreach { bt =>
            rhs.componentType = bt
          }
        case _                       =>
          ()
      }
      typed(valdef.rhs).asInstanceOf[Expr]
    }
  }

  /**
   * Given a symbol of a tree that is of type an array, this method
   * returns its component-type
   *
   * @param symbol he symbol of the tree
   */
  protected def getComponentType(
      symbol: Option[Symbol]): Option[() => Type] =
    symbol.flatMap(_.tpe.flatMap {
      case tpe: ArrayType        => Some(() => tpe.componentType)
      case _                     => None
    })

}

@component
trait SelectTyperComponent extends ooj.typechecker.SelectTyperComponent {
  /** @see {{{ooj.typechecker.SelectTyperComponent.isTypeUse}}} */
  override protected def isTypeUse(tree: Tree): Boolean = tree match {
    case t: UseTree => TreeUtils.isTypeUse(t)
    case _          => false
  }
}

trait ArrayCreationTyperComponent
  extends arrayj.typechecker.ArrayCreationTyperComponent {

  /** @see {{{TypeUtils.mkArrayType}}} */
  override protected def toArrayType(tpe: Type): Type =
    TypeUtils.mkArrayType(tpe)
}


@component
trait AssignTyperComponent extends ooj.typechecker.AssignTyperComponent {
  /** @see {{{ooj.typechecker.AssignTyperComponent.checkVariableLHS}}} */
  override protected def checkVariableLHS(lhs: Tree): Unit = {
    if(!TreeUtils.isArrayAccessOrVariableAccess(lhs))
      error(ASSIGNING_NOT_TO_VARIABLE,
        lhs.toString, lhs.toString, lhs.pos)
  }
  /** @see {{{ooj.typechecker.AssignTyperComponent.checkFinalReassigning}}} */
  override protected def checkFinalReassigning(lhs: Tree): Unit = {
    lhs match {
      case _: ArrayAccessApi               => ()
      case _                               => super.checkFinalReassigning(lhs)
    }
  }
}
