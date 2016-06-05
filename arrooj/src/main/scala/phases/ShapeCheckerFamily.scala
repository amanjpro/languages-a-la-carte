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

package ch.usi.inf.l3.sana.arrooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.arrooj.Nodes
import sana.primj.typechecker.{MethodDefShapeCheckerComponent => _,
                               ValDefShapeCheckerComponent => _,
                               BlockShapeCheckerComponent => _,
                               IfShapeCheckerComponent => _,
                               ForShapeCheckerComponent => _,
                               WhileShapeCheckerComponent => _,
                               CastShapeCheckerComponent => _,
                               ProgramShapeCheckerComponent => _,
                               _}
import sana.calcj.typechecker._
import sana.brokenj.typechecker.{LabelShapeCheckerComponent => _, _}
import sana.ooj.typechecker.{ClassDefShapeCheckerComponent => _,
                             MethodDefShapeCheckerComponent => _,
                             ValDefShapeCheckerComponent => _,
                             BlockShapeCheckerComponent => _,
                             LabelShapeCheckerComponent => _,
                             IfShapeCheckerComponent => _,
                             ForShapeCheckerComponent => _,
                             WhileShapeCheckerComponent => _,
                             CastShapeCheckerComponent => _,
                             _}
import sana.arrayj.typechecker.{MethodDefShapeCheckerComponent => _,
                                ArrayInitializerShapeCheckerComponent => _,
                                ArrayCreationShapeCheckerComponent => _,
                                LabelShapeCheckerComponent => _,
                                ValDefShapeCheckerComponent => _,
                                BlockShapeCheckerComponent => _,
                                IfShapeCheckerComponent => _,
                                ForShapeCheckerComponent => _,
                                WhileShapeCheckerComponent => _,
                                CastShapeCheckerComponent => _,
                                _}
import sana.arrooj.typechecker._



trait ShapeCheckerFamilyApi extends CheckerFamily[Tree] {
  self =>

  override def default = { case s => () }

  def components: List[PartialFunction[Tree, Unit]] =
    generateComponents[Tree, Unit](Nodes.nodes,
      "ShapeCheckerComponent", "check",
      """Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal,
      Select,This,New,Super,Break,Continue, ArrayCreation, ArrayAccess,
      ArrayTypeUse
      """)

  def check: Tree => Unit = family
}

case class ShapeCheckerFamily(compiler: CompilerInterface)
  extends ShapeCheckerFamilyApi
