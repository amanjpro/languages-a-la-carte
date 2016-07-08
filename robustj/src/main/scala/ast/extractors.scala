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

package ch.usi.inf.l3.sana.robustj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.brokenj
import sana.ooj
import sana.arrayj
import sana.arrooj
import sana.robustj
import sana.tiny.ast._
import sana.primj.ast.{ValDefApi, BlockApi}
import tiny.names.Name
import tiny.modifiers.Flags


trait TreeExtractors extends arrooj.ast.TreeExtractors {

  trait ThrowExtractor {
    def unapply(tree: ThrowApi): Option[Expr] = tree match {
        case null          => None
        case _             =>
          Some(tree.expr)
      }
  }

  trait TryExtractor {
    def unapply(tree: TryApi):
      Option[(BlockApi, List[CatchApi], Option[BlockApi])] = tree match {
        case null              => None
        case _                 =>
          Some((tree.tryClause, tree.catches, tree.finallyClause))
      }
  }

  trait CatchExtractor {
    def unapply(tree: CatchApi): Option[(ValDefApi, BlockApi)] = tree match {
      case null              => None
      case _                 =>
        Some((tree.eparam, tree.catchClause))
    }
  }

  trait MethodDefExtractor {
    def unapply(tree: MethodDefApi):
      Option[(Flags, UseTree, Name, List[ValDefApi], List[UseTree], Expr)] =
        tree match {
          case null          => None
          case _             =>
            Some((tree.mods, tree.ret, tree.name, tree.params,
              tree.throwsClause, tree.body))
        }

  }
}

object TreeExtractors extends TreeExtractors {
  val TypeUse          = new TypeUseExtractor {}
  val Ident            = new IdentExtractor {}

  val Cast             = new CastExtractor {}
  val Literal          = new LiteralExtractor {}
  val Binary           = new BinaryExtractor {}
  val Unary            = new UnaryExtractor {}


  val Block            = new BlockExtractor {}
  val Assign           = new AssignExtractor {}
  val If               = new IfExtractor {}
  val While            = new WhileExtractor {}
  val For              = new ForExtractor {}
  val Ternary          = new TernaryExtractor {}
  val Apply            = new ApplyExtractor {}
  val Return           = new ReturnExtractor {}
  val ValDef           = new ValDefExtractor {}


  val Label            = new LabelExtractor {}
  val Break            = new BreakExtractor {}
  val Continue         = new ContinueExtractor {}
  val Case             = new CaseExtractor {}
  val Switch           = new SwitchExtractor {}

  val ArrayInitializer = new ArrayInitializerExtractor {}
  val ArrayAccess      = new ArrayAccessExtractor {}
  val ArrayTypeUse     = new ArrayTypeUseExtractor {}
  val ArrayCreation    = new ArrayCreationExtractor {}

  val CompilationUnit  = new CompilationUnitExtractor {}
  val PackageDef       = new PackageDefExtractor {}
  val ClassDef         = new ClassDefExtractor {}
  val Template         = new TemplateExtractor {}
  val New              = new NewExtractor {}
  val Select           = new SelectExtractor {}


  val Try              = new TryExtractor {}
  val Throw            = new ThrowExtractor {}
  val Catch            = new CatchExtractor {}
  val MethodDef        = new MethodDefExtractor {}
}
