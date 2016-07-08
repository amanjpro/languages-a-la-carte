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

package ch.usi.inf.l3.sana.dcct.typechecker


import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.dcct
import sana.ooj

import tiny.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.names.Name
import primj.ast.Implicits._
import tiny.types.{TypeUtils => _, _}
import tiny.symbols.{Symbol, TypeSymbol, TermSymbol}
import tiny.source.Position
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.{TyperComponent, TypePromotions}
import calcj.types._
import calcj.ast.UnaryApi
import calcj.ast.operators._
import primj.ast.{TreeCopiers => _, _}
import primj.ast.TreeFactories._
import primj.symbols.{VariableSymbol, MethodSymbol, SymbolUtils}
import dcct.errors.ErrorCodes._
import primj.types._
import primj.modifiers.Ops._
import dcct.ast._
import dcct.types._
import ooj.ast.ClassDefApi


@component
trait ClassDefTyperComponent extends ooj.typechecker.ClassDefTyperComponent {
  override protected def checkParents(parents: List[UseTree],
      clazz: ClassDefApi): Unit = {}
}

@component
trait ValDefTyperComponent extends ooj.typechecker.ValDefTyperComponent {
  (mthd: ValDefApi) => {
    val res = super.apply(mthd).asInstanceOf[ValDefApi]
    res.tpt.tpe match {
      case Some(_: CloudType) if !res.mods.isField =>
        error(CLOUDY_VARIABLE_TYPE, "", "", mthd.pos)
      case _                                       => ()
    }
    res
  }
}

@component
trait MethodDefTyperComponent extends ooj.typechecker.MethodDefTyperComponent {
  (mthd: MethodDefApi) => {
    val res = super.apply(mthd).asInstanceOf[MethodDefApi]
    res.ret.tpe match {
      case Some(_: CloudType) => error(CLOUDY_RETURN_TYPE, "", "", mthd.pos)
      case _                  => ()
    }
    res
  }
}

@component
trait ArrayDefTyperComponent extends TyperComponent {
  (array: ArrayDefApi) => {
    val indices = array.indices.map(typed(_).asInstanceOf[ValDefApi])
    val properties = array.properties.map(typed(_).asInstanceOf[ValDefApi])
    TreeCopiers.copyArrayDef(array)(indices = indices, properties = properties)
  }
}

@component
trait ForEachTyperComponent extends TyperComponent {
  (foreach: ForEachApi)     => {
    val entityVar = typed(foreach.entityVar).asInstanceOf[ValDefApi]
    val whereExpr = typed(foreach.whereExpr).asInstanceOf[Expr]
    val body      = typed(foreach.body).asInstanceOf[BlockApi]
    TreeCopiers.copyForEach(foreach)(entityVar = entityVar,
      whereExpr = whereExpr, body = body)
  }
}
