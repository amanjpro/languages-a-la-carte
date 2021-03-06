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

package ch.usi.inf.l3.sana.primj.types

import ch.usi.inf.l3.sana
import sana.primj.names.StdNames
import sana.calcj.types._
import sana.calcj.typechecker.TypePromotions
import sana.tiny.types.Type
import sana.tiny.ast.{Tree, Expr}
import sana.tiny.ast.Implicits._
import sana.primj.symbols.SymbolUtils


trait TypeUtils extends sana.tiny.types.TypeUtils {


  /**
   * Checks if a type is assignable to anther.
   *
   * @param rtree the tree which has the type {{{rtpe}}}.
   * @param rtpe  the type of which we want to be assignable
   * @param ltpe the type of which we want to assign {{{rtpe}}} to
   */
  def isAssignable(rtree: Tree, rtpe: Type, ltpe: Type): Boolean = {
    (ltpe, rtpe) match {
      case (ShortType, IntType) |
           (CharType, IntType)  |
           (ByteType, IntType)           =>
        TypePromotions.isNarrawableTo(rtree, ltpe)
      case _                             => rtpe <:< ltpe
    }
  }


  /**
   * Unifies the two types of the "then clause" and "else clause" of a ternary
   * operation.
   *
   * @param lhs the {{{thenp}}} expression of a ternary expression
   * @param rhs the {{{elsep}}} expression of a ternary expression
   * @return if the two types are unifiable, return the unified type or return
   *         {{{None}}}. Two types are unifiable if they have a common supertype.
   */
  def unifyTernaryBranches(lhs: Expr, rhs: Expr): Option[Type] = {
    (lhs.tpe, rhs.tpe) match {
      case (Some(t1), Some(t2)) if t1 =:= t2              =>
        Some(t1)
      case (Some(ByteType), Some(ShortType))              =>
        Some(ShortType)
      case (Some(ShortType), Some(ByteType))              =>
        Some(ShortType)
      case (Some(tpe1: NumericType),
            Some(tpe2: NumericType))                      =>
        if(TypePromotions.isNarrawableTo(rhs, tpe1)) {
          Some(tpe1)
        }
        else if(TypePromotions.isNarrawableTo(lhs, tpe2)) {
          Some(tpe2)
        } else {
          // INFO: This will be extended once we have OOJ
          Some(TypePromotions.binaryNumericPromotion(tpe1, tpe2))
        }
      case _                                              => None
    }
  }
}



object TypeUtils extends TypeUtils
