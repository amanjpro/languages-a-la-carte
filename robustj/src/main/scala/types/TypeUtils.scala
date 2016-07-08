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

package ch.usi.inf.l3.sana.robustj.types

import ch.usi.inf.l3.sana
import sana.calcj.types._
import sana.tiny.types.Type
import sana.tiny.ast.Expr
import sana.tiny.ast.Implicits._
import sana.ooj.types.{ClassTypeApi, ClassType}
import sana.robustj.names.StdNames

trait TypeUtils extends sana.arrooj.types.TypeUtils {
  lazy val throwableClassType: ClassTypeApi        = {
    val qual            = javaLangPackageName
    val name            = StdNames.THROWABLE_CLASS_NAME
    ClassType(qual, name, Set.empty)
  }
  lazy val errorClassType: ClassTypeApi            = {
    val qual            = javaLangPackageName
    val name            = StdNames.ERROR_CLASS_NAME
    ClassType(qual, name, Set.empty)
  }
  lazy val exceptionClassType: ClassTypeApi        = {
    val qual            = javaLangPackageName
    val name            = StdNames.EXCEPTION_CLASS_NAME
    ClassType(qual, name, Set.empty)
  }
  lazy val runtimeExceptionClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.RUNTIME_EXCEPTION_CLASS_NAME
    ClassType(qual, name, Set.empty)
  }


  def isCheckedException(tpe: Option[Type]): Boolean = tpe.map { tpe =>
    !(tpe <:< runtimeExceptionClassType) && tpe <:< exceptionClassType
  }.getOrElse(false)

  def isDefinitivelyCheckedException(p: Option[Type]): Boolean = p.map { tpe =>
    isCheckedException(p) && tpe =/= exceptionClassType
  }.getOrElse(false)
}

object TypeUtils extends TypeUtils
