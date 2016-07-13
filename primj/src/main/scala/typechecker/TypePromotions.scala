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

package ch.usi.inf.l3.sana.primj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny.ast.Expr
import sana.tiny.ast.Implicits._
import sana.primj.ast.TreeFactories._
import sana.tiny.types._
import sana.calcj.types._
import sana.calcj.ast._
import sana.primj.symbols.SymbolUtils


trait TypePromotions extends sana.calcj.typechecker.TypePromotions {
  /**
   * Given an expression, and its expected type it widens the expression
   * to the type if possible and needed. It is possible if the expected
   * type is primitive long, float, or double; and, it is needed is the
   * type of the expression is a subtype of the expected type but not
   * equal to it.
   *
   * @param expr the expression to be widened
   * @param otpe the expected type to widen expr to
   */
  def widenIfNeeded(expr: Expr, otpe: Option[Type]): Expr = {
    val res = for {
      etpe <- expr.tpe
      tpe  <- otpe      if etpe <:< tpe &&
                             etpe =/= tpe &&
                             tpe >:> IntType &&
                             tpe =/= IntType &&
                             etpe.isInstanceOf[PrimitiveType]
      sym  <- SymbolUtils.getSymbol(tpe)
    } yield {
      val tuse = TreeFactories.mkTypeUse(sym.name, expr.pos)
      val cast = TreeFactories.mkCast(tuse, expr, expr.pos)
      expr.owner.foreach { owner =>
        tuse.owner = owner
        cast.owner = owner
      }
      cast
    }
    res.getOrElse(expr)
  }
}

object TypePromotions extends TypePromotions
