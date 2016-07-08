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

package ch.usi.inf.l3.sana.calcj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny.ast._
import sana.tiny.ast.Implicits._
import sana.calcj.ast.TreeFactories._
import sana.tiny.types._
import sana.calcj.types._
import sana.calcj.ast._
import sana.calcj.ast.TreeExtractors._
import sana.calcj.symbols.SymbolUtils._


trait TypePromotions {
  /**
   * Performs the Java's binary promotion for two numeric types.
   *
   * @param t1 the first type
   * @param t2 the second type
   */
  def binaryNumericPromotion(t1: NumericType,
    t2: NumericType): PrimitiveType = (t1, t2) match {
      case (DoubleType, _) => DoubleType
      case (_, DoubleType) => DoubleType
      case (FloatType, _)  => FloatType
      case (_, FloatType)  => FloatType
      case (LongType, _)   => LongType
      case (_, LongType)   => LongType
      case (_, _)          => IntType
    }

  /**
   * Performs the Java's unary promotion a numeric type.
   *
   * @param t1 the numeric type
   */
  def unaryNumericPromotion(t1: NumericType): NumericType = t1 match {
    case LongType        => LongType
    case x: IntegralType => IntType
    case _               => t1
  }



  /**
   * Given an expression, and an actual type and the expected type, it
   * casts the expression from the actual type to the expected type should
   * they not be the same.
   *
   * @param e the expression to be casted if needed
   * @param t1 the expected type
   * @param t2 the actual type
   */
  def castIfNeeded(e: Expr, t1: Type, t2: Type): Expr = {
    if(t1 =:= t2) e
    else {
      val pos = e.pos
      getSymbol(t1) match {
        case Some(sym) =>
          val tuse = mkTypeUse(sym.name, pos)
          mkCast(tuse, e, pos = pos)
        case _         =>
          e
      }
    }
  }


  /**
   * Checks if a tree (of type literal) can be narrowed to a type.
   *
   * @param expr the expression to be checked
   * @param tpe the type to which we check {{{expr}}} to be narrowed to.
   */
  def isNarrawableTo(expr: Tree, tpe: Type): Boolean = expr match {
    case Literal(c) if c.tpe =:= IntType =>
      val value = c.value.asInstanceOf[Int]
      if(tpe =:= ShortType) {
        value.isValidShort
      } else if(tpe =:= ByteType) {
        value.isValidByte
      } else if(tpe =:= CharType) {
        value.isValidChar
      } else false
    case _                               =>
      false
  }
}


object TypePromotions extends TypePromotions
