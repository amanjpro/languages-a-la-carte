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

package ch.usi.inf.l3.sana.calcj.ast


object operators {

  /** The supertype of the operators supported by this module */
  trait Op


  // Unary Operators

  /** The supertype of the unary operators supported by this module */
  trait UOp extends Op

  /** The supertype of the postfix unary-operators supported by this module */
  trait POp extends Op

  /** Indicates ! in Java */
  object Not extends UOp {
    override def toString: String = "!"
  }

  /** Indicates ~ in Java */
  object BCompl extends UOp {
    override def toString: String = "~"
  }

  /** Indicates ++ in Java */
  object Inc extends UOp with POp {
    override def toString: String = "++"
  }

  /** Indicates -- in Java */
  object Dec extends UOp with POp {
    override def toString: String = "--"
  }

  /** Indicates the unary - in Java */
  object Neg extends UOp {
    override def toString: String = "-"
  }

  /** Indicates the unary + in Java */
  object Pos extends UOp {
    override def toString: String = "+"
  }

  // Binary Operators

  /** The supertype of the binary operators supported by this module */
  trait BOp extends Op

  /** Indicates > in Java */
  object Gt extends BOp {
    override def toString: String = ">"
  }
  /** Indicates < in Java */
  object Lt extends BOp {
    override def toString: String = "<"
  }

  /** Indicates == in Java */
  object Eq extends BOp {
    override def toString: String = "=="
  }
  /** Indicates <= in Java */
  object Le extends BOp {
    override def toString: String = "<="
  }
  /** Indicates >= in Java */
  object Ge extends BOp {
    override def toString: String = ">="
  }
  /** Indicates != in Java */
  object Neq extends BOp {
    override def toString: String = "!="
  }
  /** Indicates && in Java */
  object And extends BOp {
    override def toString: String = "&&"
  }
  /** Indicates || in Java */
  object Or extends BOp {
    override def toString: String = "||"
  }
  /** Indicates binary + in Java */
  object Add extends BOp {
    override def toString: String = "+"
  }
  /** Indicates binary - in Java */
  object Sub extends BOp {
    override def toString: String = "-"
  }
  /** Indicates * in Java */
  object Mul extends BOp {
    override def toString: String = "*"
  }
  /** Indicates / in Java */
  object Div extends BOp {
    override def toString: String = "/"
  }
  /** Indicates & in Java */
  object BAnd extends BOp {
    override def toString: String = "&"
  }
  /** Indicates | in Java */
  object BOr extends BOp {
    override def toString: String = "|"
  }
  /** Indicates ^ in Java */
  object BXor extends BOp {
    override def toString: String = "^"
  }
  /** Indicates % in Java */
  object Mod extends BOp {
    override def toString: String = "%"
  }
  /** Indicates << in Java */
  object SHL extends BOp {
    override def toString: String = "<<"
  }
  /** Indicates >> in Java */
  object SHR extends BOp {
    override def toString: String = ">>"
  }
  /** Indicates >>> in Java */
  object USHR extends BOp {
    override def toString: String = ">>>"
  }
}
