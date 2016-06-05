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

package ch.usi.inf.l3.sana.oberon0.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.arrayj
import sana.ooj

import tiny.ast._
import tiny.ast.Implicits._
import primj.ast.{BlockApi}
import tiny.names.Name
import ooj.ast.{TemplateApi, ClassDefApi}
import arrayj.ast.{TreeExtractors => ATreeExtractors}
import ooj.ast.{TreeExtractors => OTreeExtractors}



trait TreeExtractors extends primj.ast.TreeExtractors {

  trait ModuleDefExtractor {
    def unappy(tree: ModuleDefApi): Option[(Name, List[DefTree], Option[BlockApi])] =
      tree match {
        case null  => None
        case _     => Some((tree.name, tree.declarations, tree.block))
      }
  }

  trait TypeDefExtractor {
    def unapply(tree: TypeDefApi): Option[(Name, Tree)] = tree match {
      case null    => None
      case _       => Some((tree.name, tree.tpt))
    }
  }


  trait ArrayTypeUseExtractor {
    def unapply(tree: ArrayTypeUseApi): Option[(UseTree, Expr)] = tree match {
      case null    => None
      case _       => Some((tree.tpt, tree.size))
    }
  }



  trait ArrayAccessExtractor
    extends ATreeExtractors.ArrayAccessExtractor


  trait RecordDefExtractor {
    def unapply(tree: ClassDefApi): Option[TemplateApi] = tree match {
      case null    => None
      case _       => Some(tree.body)
    }
  }

  trait TemplateExtractor
    extends OTreeExtractors.TemplateExtractor

  trait SelectExtractor
    extends OTreeExtractors.SelectExtractor
}

object TreeExtractors extends TreeExtractors {
  val TypeUse          = new TypeUseExtractor {}
  val Ident            = new IdentExtractor {}

  val Literal          = new LiteralExtractor {}
  val Binary           = new BinaryExtractor {}
  val Unary            = new UnaryExtractor {}


  val Block            = new BlockExtractor {}
  val Assign           = new AssignExtractor {}
  val If               = new IfExtractor {}
  val While            = new WhileExtractor {}
  val Apply            = new ApplyExtractor {}
  val ValDef           = new ValDefExtractor {}


  val ArrayAccess      = new ArrayAccessExtractor {}
  val ArrayTypeUse     = new ArrayTypeUseExtractor {}

  val RecordDef        = new RecordDefExtractor {}
  val Template        = new TemplateExtractor {}
  val Select          = new SelectExtractor {}
  val MethodDef       = new MethodDefExtractor {}



  val ModuleDef = new ModuleDefExtractor {}
  val TypeDef   = new TypeDefExtractor {}
}
