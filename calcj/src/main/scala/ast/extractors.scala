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


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.tiny.ast._
import tiny.names.Name
import operators._


trait TreeExtractors extends tiny.ast.TreeExtractors {


  trait CastExtractor {
    def unapply(cast: CastApi): Option[(UseTree, Expr)] = cast match {
      case null          => None
      case _             => Some((cast.tpt, cast.expr))
    }
  }


  trait LiteralExtractor {
    def unapply(lit: LiteralApi): Option[Constant] = lit match {
      case null          => None
      case _             => Some(lit.constant)
    }
  }


  trait BinaryExtractor {
    def unapply(bin: BinaryApi): Option[(Expr, BOp, Expr)] = bin match {
      case null          => None
      case _             => Some((bin.lhs, bin.op, bin.rhs))
    }
  }

  trait UnaryExtractor {
    def unapply(unary: UnaryApi): Option[(Boolean, UOp, Expr)] = unary match {
      case null          => None
      case _             => Some((unary.isPostfix, unary.op, unary.expr))
    }
  }
}


object TreeExtractors extends TreeExtractors {
  val TypeUse = new TypeUseExtractor {}
  val Ident   = new IdentExtractor {}

  val Cast    = new CastExtractor {}
  val Literal = new LiteralExtractor {}
  val Binary  = new BinaryExtractor {}
  val Unary   = new UnaryExtractor {}
}
