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

package ch.usi.inf.l3.sana.ooj.types

import ch.usi.inf.l3.sana
import sana.ooj.names.StdNames
import sana.calcj.types._
import sana.tiny.types.Type
import sana.tiny.ast.Expr
import sana.tiny.ast.Implicits._
import sana.ooj.symbols.SymbolUtils

trait TypeUtils extends sana.primj.types.TypeUtils {
  protected def javaLangPackageName: String = {
    val java = StdNames.JAVA_PACKAGE_NAME.asString
    val lang = StdNames.LANG_PACKAGE_NAME.asString
    s"$java.$lang"
  }
  lazy val objectClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.OBJECT_TYPE_NAME
    ClassType(qual, name, Set.empty)
  }

  lazy val stringClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.STRING_TYPE_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val booleanClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.BOOLEAN_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val characterClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.CHARACTER_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val integerClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.INTEGER_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val longClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.LONG_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val floatClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.FLOAT_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val doubleClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.DOUBLE_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  override def unifyTernaryBranches(lhs: Expr, rhs: Expr): Option[Type] = {
    (lhs.tpe, rhs.tpe) match {
      case (Some(NullType), Some(tpe))                                 =>
        Some(tpe)
      case (Some(tpe), Some(NullType))                                 =>
        Some(tpe)
      case (Some(tpe1: NumericType),
            Some(tpe2: NumericType))                                   =>
        super.unifyTernaryBranches(lhs, rhs)
      case (Some(ltpe), Some(rtpe))                                    =>
        if(ltpe <:< rtpe)      Some(rtpe)
        else if(rtpe <:< ltpe) Some(ltpe)
        else                   None
      case _                                                           =>
        super.unifyTernaryBranches(lhs, rhs)
    }
  }

  def toBoxedType(tpe: Type): Option[Type] = tpe match {
    case BooleanType            =>
      Some(booleanClassType)
    case CharType               =>
      Some(characterClassType)
    case IntType                =>
      Some(integerClassType)
    case LongType               =>
      Some(longClassType)
    case FloatType              =>
      Some(floatClassType)
    case DoubleType             =>
      Some(doubleClassType)
    case _                      =>
      None
  }
}


object TypeUtils extends TypeUtils
