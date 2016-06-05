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

package ch.usi.inf.l3.sana.primj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.tiny.ast._
import tiny.names.Name
import tiny.modifiers.Flags


trait TreeExtractors extends calcj.ast.TreeExtractors {


  trait ProgramExtractor {
    def unapply(tree: ProgramApi): Option[(List[DefTree], String)] = tree match {
      case null          => None
      case _             => Some((tree.members, tree.sourceName))
    }
  }

  trait AssignExtractor {
    def unapply(tree: AssignApi): Option[(Expr, Expr)] = tree match {
      case null          => None
      case _             => Some((tree.lhs, tree.rhs))
    }
  }

  trait IfExtractor {
    def unapply(tree: IfApi): Option[(Expr, Expr, Expr)] = tree match {
      case null          => None
      case _             => Some((tree.cond, tree.thenp, tree.elsep))
    }
  }


  trait WhileExtractor {
    def unapply(tree: WhileApi): Option[(Expr, Expr)] = tree match {
      case null          => None
      case _             => Some((tree.cond, tree.body))
    }
  }


  trait ForExtractor {
    def unapply(tree: ForApi): Option[(List[Tree], Expr, List[Expr], Expr)] =
      tree match {
        case null          => None
        case _             => Some((tree.inits, tree.cond, tree.steps, tree.body))
      }
  }

  trait TernaryExtractor {
    def unapply(tree: TernaryApi): Option[(Expr, Expr, Expr)] = tree match {
      case null          => None
      case _             => Some((tree.cond, tree.thenp, tree.elsep))
    }
  }


  trait ApplyExtractor {
    def unapply(tree: ApplyApi): Option[(Expr, List[Expr])] = tree match {
      case null          => None
      case _             => Some((tree.fun, tree.args))
    }
  }

  trait ReturnExtractor {
    def unapply(tree: ReturnApi): Option[Option[Expr]] = tree match {
      case null          => None
      case _             => Some((tree.expr))
    }
  }

  trait BlockExtractor {
    def unapply(tree: BlockApi): Option[List[Tree]] = tree match {
      case null          => None
      case _             => Some((tree.stmts))
    }
  }

  trait MethodDefExtractor {
    def unapply(tree: MethodDefApi):
      Option[(UseTree, Name, List[ValDefApi], Expr)] = tree match {
      case null          => None
      case _             => Some((tree.ret, tree.name, tree.params, tree.body))
    }
  }

  trait ValDefExtractor {
    def unapply(tree: ValDefApi): Option[(Flags, UseTree, Name, Expr)] = tree match {
      case null          => None
      case _             => Some((tree.mods, tree.tpt, tree.name, tree.rhs))
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
  val Block     = new BlockExtractor {}
  val MethodDef = new MethodDefExtractor {}
  val ValDef    = new ValDefExtractor {}
}
