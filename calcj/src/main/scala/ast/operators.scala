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


object operators {
  trait Op


  // Unary Operators

  trait UOp extends Op

  trait POp extends Op

  object Not extends UOp {
    override def toString: String = "!"
  }
  object BCompl extends UOp {
    override def toString: String = "~"
  }
  object Inc extends UOp with POp {
    override def toString: String = "++"
  }
  object Dec extends UOp with POp {
    override def toString: String = "--"
  }
  object Neg extends UOp {
    override def toString: String = "-"
  }
  object Pos extends UOp {
    override def toString: String = "+"
  }

  // Binary Operators
  trait BOp extends Op

  object Gt extends BOp {
    override def toString: String = ">"
  }
  object Lt extends BOp {
    override def toString: String = "<"
  }

  object Eq extends BOp {
    override def toString: String = "=="
  }
  object Le extends BOp {
    override def toString: String = "<="
  }
  object Ge extends BOp {
    override def toString: String = ">="
  }
  object Neq extends BOp {
    override def toString: String = "!="
  }
  object And extends BOp {
    override def toString: String = "&&"
  }
  object Or extends BOp {
    override def toString: String = "||"
  }
  object Add extends BOp {
    override def toString: String = "+"
  }
  object Sub extends BOp {
    override def toString: String = "-"
  }
  object Mul extends BOp {
    override def toString: String = "*"
  }
  object Div extends BOp {
    override def toString: String = "/"
  }
  object BAnd extends BOp {
    override def toString: String = "&"
  }
  object BOr extends BOp {
    override def toString: String = "|"
  }
  object BXor extends BOp {
    override def toString: String = "^"
  }
  object Mod extends BOp {
    override def toString: String = "%"
  }
  object SHL extends BOp {
    override def toString: String = "<<"
  }
  object SHR extends BOp {
    override def toString: String = ">>"
  }
  object USHR extends BOp {
    override def toString: String = ">>>"
  }
}
