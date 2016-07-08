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

package ch.usi.inf.l3.sana.brokenj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.tiny.ast._
import tiny.names.Name


trait TreeExtractors extends primj.ast.TreeExtractors {


  trait LabelExtractor {
    def unapply(tree: LabelApi): Option[(Name, Expr)] = tree match {
      case null          => None
      case _             => Some((tree.name, tree.stmt))
    }
  }

  trait BreakExtractor {
    def unapply(tree: BreakApi): Option[Option[Name]] = tree match {
      case null          => None
      case _             => Some(tree.label)
    }
  }

  trait ContinueExtractor {
    def unapply(tree: ContinueApi): Option[Option[Name]] = tree match {
      case null          => None
      case _             => Some(tree.label)
    }
  }


  trait CaseExtractor {
    def unapply(tree: CaseApi): Option[(List[Expr], Tree)] = tree match {
      case null          => None
      case _             => Some((tree.guards, tree.body))
    }
  }


  trait SwitchExtractor {
    def unapply(tree: SwitchApi): Option[(Expr, List[CaseApi])] =
      tree match {
        case null          => None
        case _             => Some((tree.expr, tree.cases))
      }
  }

}


object TreeExtractors extends TreeExtractors {
  val TypeUse   = new TypeUseExtractor {}
  val Ident     = new IdentExtractor {}

  val Cast      = new CastExtractor {}
  val Literal   = new LiteralExtractor {}
  val Binary    = new BinaryExtractor {}
  val Unary     = new UnaryExtractor {}


  val Program   = new ProgramExtractor {}
  val Assign    = new AssignExtractor {}
  val If        = new IfExtractor {}
  val While     = new WhileExtractor {}
  val For       = new ForExtractor {}
  val Ternary   = new TernaryExtractor {}
  val Apply     = new ApplyExtractor {}
  val Return    = new ReturnExtractor {}
  val MethodDef = new MethodDefExtractor {}
  val ValDef    = new ValDefExtractor {}


  val Label     = new LabelExtractor {}
  val Break     = new BreakExtractor {}
  val Continue  = new ContinueExtractor {}
  val Case      = new CaseExtractor {}
  val Switch    = new SwitchExtractor {}

}
