/*
 * Copyright (c) <2015-2016>, see CONTRIBUTERS
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

package ch.usi.inf.l3.sana.dcct.namers

import ch.usi.inf.l3.sana

import sana.ooj
import sana.tiny
import sana.calcj
import sana.primj
import sana.dcct
import tiny.dsl._

import tiny.ast.{TreeCopiers => _, _}
import calcj.ast.{TreeCopiers => _, _}
import primj.ast.{TreeCopiers => _, _}
import primj.ast.ProgramApi
import tiny.ast.DefTree
import ooj.ast.{ClassDefApi, TemplateApi}
import ooj.symbols.ClassSymbol
import ooj.ast.Implicits._
import dcct.symbols._
import dcct.ast._
import primj.namers.SymbolAssignerComponent
import primj.symbols.ScopeSymbol


@component
trait ClassDefSymbolAssignerComponent extends ooj.namers.ClassDefSymbolAssignerComponent {
  
  override protected def addDefaultConstructor(clazz: ClassDefApi, 
   sym: ClassSymbol ): TemplateApi = {
    clazz.body.owner = sym
    clazz.body
  }
}

@component
trait ArrayDefSymbolAssignerComponent extends SymbolAssignerComponent {
  // TODO why do I need the arraySymbol anyway???
  (array: ArrayDefApi) => { 
    val symbol = ArraySymbol(array.name, array.owner)
    val indices = array.indices.map { x =>
      x.owner = symbol
      assign(x).asInstanceOf[ValDefApi]
    }
    val properties = array.properties.map { x =>
      x.owner = symbol
      assign(x).asInstanceOf[ValDefApi]
    }
    array.symbol = symbol
    TreeCopiers.copyArrayDef(array)(indices = indices, properties = properties)
  }
}

@component 
trait ForEachSymbolAssignerComponent extends SymbolAssignerComponent {
  (foreach: ForEachApi) => {
    val owner = foreach.owner
    val symbol = ScopeSymbol(owner)
    foreach.entityVar.owner = symbol
    foreach.body.owner = symbol
    val entityVar = assign(foreach.entityVar).asInstanceOf[ValDefApi]
    val whereExpr = assign(foreach.whereExpr).asInstanceOf[Expr]
    val body      = assign(foreach.body).asInstanceOf[BlockApi]
    foreach.symbol = symbol
    TreeCopiers.copyForEach(foreach)(entityVar = entityVar,
      whereExpr = whereExpr, body = body)
  }
}

