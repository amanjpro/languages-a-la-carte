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

package ch.usi.inf.l3.sana.calcj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj

import tiny.ast._
import calcj.ast._

import calcj.ast.operators._
import calcj.types._
import tiny.types._

trait TreeUtils {
  def narrowDown(lit: LiteralApi, tpe: Type): LiteralApi = {
    tpe match {
      case ShortType =>
        TreeCopiers.copyLiteral(lit)(constant = {
          ShortConstant(lit.constant.value.toString.toShort)
        })
      case CharType  =>
        TreeCopiers.copyLiteral(lit)(constant = {
          CharConstant(lit.constant.value.asInstanceOf[Int].toChar)
        })
      case ByteType  =>
        TreeCopiers.copyLiteral(lit)(constant = {
          ByteConstant(lit.constant.value.toString.toByte)
        })
      case _         =>
        lit
    }
  }


  def widen(lit: LiteralApi, tpe: Type): LiteralApi = {
    tpe match {
      case IntType         =>
        TreeCopiers.copyLiteral(lit)(constant = {
          lit.constant.value match {
            case ch: Char      => IntConstant(ch.toInt)
            case _             =>
              IntConstant(lit.constant.value.toString.toInt)
          }
        })
      case LongType        =>
        TreeCopiers.copyLiteral(lit)(constant = {
          lit.constant.value match {
            case ch: Char      => LongConstant(ch.toLong)
            case _             =>
              LongConstant(lit.constant.value.toString.toLong)
          }
        })
      case FloatType       =>
        TreeCopiers.copyLiteral(lit)(constant = {
          lit.constant.value match {
            case ch: Char      => FloatConstant(ch.toFloat)
            case _             =>
              FloatConstant(lit.constant.value.toString.toFloat)
          }
        })
      case DoubleType      =>
        TreeCopiers.copyLiteral(lit)(constant = {
          lit.constant.value match {
            case ch: Char      => DoubleConstant(ch.toDouble)
            case _             =>
              DoubleConstant(lit.constant.value.toString.toDouble)
          }
        })
      case _               =>
        lit
    }
  }
}
