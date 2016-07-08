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

package ch.usi.inf.l3.sana.ppj.typechecker

import ch.usi.inf.l3.sana
import sana.ppj
import sana.dynj
import sana.robustj
import sana.arrooj
import sana.ooj
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj


import tiny.dsl._
import calcj.typechecker.TyperComponent
import tiny.ast.{Tree, Expr, NoTree}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast.BlockApi
import tiny.types.Type
import ppj.ast._
import ppj.errors.ErrorCodes._
import robustj.types.TypeUtils
import ppj.modifiers.Ops._
import arrooj.ast.Implicits._


@component
trait MethodDefTyperComponent
  extends robustj.typechecker.MethodDefTyperComponent {

  override def allPathsReturn(expr: Tree): Boolean = {
    enclosingMethod(expr.symbol) match {
      case Some(mthd)                         =>
        mthd.mods.isAbstract || TreeUtils.allPathsReturn(expr)
      case None                               =>
        expr == NoTree
    }
  }
}



@component
trait SynchronizedTyperComponent extends TyperComponent {
  (sync: SynchronizedApi) => {
    val expr = typed(sync.expr).asInstanceOf[Expr]
    val block = typed(sync.block).asInstanceOf[BlockApi]

    expr.tpe match {
      case Some(tpe)    if tpe <:< objectClassType => ()
      case _                                       =>
        error(REFERENCE_TYPE_EXPECTED, "", "", expr.pos)
    }

    TreeCopiers.copySynchronized(sync)(expr, block)
  }


  protected def objectClassType: Type =
    TypeUtils.objectClassType
}
