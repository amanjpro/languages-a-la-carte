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

package ch.usi.inf.l3.sana.dynj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.ooj
import sana.dynj
import sana.robustj


import tiny.dsl._

import tiny.errors.ErrorReporting.{error,warning}
import tiny.ast.Implicits._
import tiny.ast.{Tree, UseTree, Expr}
import tiny.types.{ErrorType, Type}
import calcj.ast.{BinaryApi, CastApi}
import calcj.types.{BooleanType, NumericType}
import ooj.types.RefType
import dynj.errors.ErrorCodes._
import dynj.ast.operators.InstanceOf
import robustj.ast.TreeCopiers
import calcj.typechecker.TyperComponent
import robustj.types.TypeUtils

@component
trait BinaryTyperComponent extends ooj.typechecker.BinaryTyperComponent {

  override protected def binaryTyper(ltpe: Type,
    rtpe: Type, bin: BinaryApi): Option[(Type, Type, Type)] = bin.op match {
    case InstanceOf                                 =>
      if(ltpe.isInstanceOf[RefType] && rtpe.isInstanceOf[RefType]) {
        Some((ltpe, rtpe, BooleanType))
      } else {
        error(DYNAMIC_TYPE_CHECK_NONE_REF_TYPE, "", "", bin.pos)
        None
      }
    case _                                          =>
      super.binaryTyper(ltpe, rtpe, bin)
  }

}


@component
trait CastTyperComponent extends TyperComponent {
  (cast: CastApi)           => {
    val tpt  = typed(cast.tpt).asInstanceOf[UseTree]
    val expr = typed(cast.expr).asInstanceOf[Expr]
    tpt.symbol.foreach(cast.symbol = _)
    val res: Option[Tree]  = for {
      ttpe <- tpt.tpe
      etpe <- expr.tpe
    } yield {
      if(ttpe <:< objectClassType &&
          etpe <:< objectClassType) {
        if(ttpe <:< etpe || etpe <:< ttpe) {
          tpt.tpe.foreach(cast.tpe = _)
          TreeCopiers.copyCast(cast)(tpt, expr)
        } else {
          error(TYPE_MISMATCH, "", "", cast.pos)
          val r = TreeCopiers.copyCast(cast)(tpt, expr)
          r.tpe = ErrorType
          r
        }
      } else if(ttpe.isInstanceOf[NumericType] &&
                etpe.isInstanceOf[NumericType]) {
        tpt.tpe.foreach(cast.tpe = _)
        TreeCopiers.copyCast(cast)(tpt, expr)
      } else if(ttpe <:< BooleanType &&
                etpe <:< BooleanType) {
        tpt.tpe.foreach(cast.tpe = _)
        TreeCopiers.copyCast(cast)(tpt, expr)
      } else {
        error(TYPE_MISMATCH, "", "", cast.pos)
        val r = TreeCopiers.copyCast(cast)(tpt, expr)
        r.tpe = ErrorType
        r
      }
    }
    res.getOrElse(cast)
  }


  /** @see {{{TypeUtils.objectClassType}}} */
  protected def objectClassType: Type =
    TypeUtils.objectClassType

}
