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

package ch.usi.inf.l3.sana.ooj.typechecker

import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.CheckerComponent
import tiny.dsl._
import primj.ast.Implicits._
import primj.ast.{MethodDefApi => PMethodDefApi}
import tiny.names.Name
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.symbols._
import primj.modifiers.Ops._
import primj.typechecker.ShapeCheckerComponent
import brokenj.typechecker.JumpCheckerComponent
import brokenj.errors.ErrorCodes._
import brokenj.ast.TreeUtils

import ooj.ast._



/**
CompilationUnit: DONE
Program: DONE
PackageDef: DONE
ClassDef: DONE
Template: DONE
New: DONE
Select: DONE
This: DONE
Super: DONE
MethodDef: DONE
*/

@component(tree, encls)
trait ProgramJumpCheckerComponent extends JumpCheckerComponent {
  (prg: ProgramApi)  => {
    prg.members.foreach { member => check((member, Nil)) }
  }
}

@component(tree, encls)
trait CompilationUnitJumpCheckerComponent extends
    JumpCheckerComponent {
  (cunit: CompilationUnitApi)  => {
    check((cunit.module, encls))
  }
}

@component(tree, encls)
trait PackageDefJumpCheckerComponent extends JumpCheckerComponent {
  (pkg: PackageDefApi)  => {
    pkg.members.foreach { member => check((member, encls)) }
  }
}

@component(tree, encls)
trait ClassDefJumpCheckerComponent extends JumpCheckerComponent {
  (clazz: ClassDefApi)  => {
    clazz.parents.foreach { parent => check((parent, encls)) }
    check((clazz.body, encls))
  }
}

@component(tree, encls)
trait TemplateJumpCheckerComponent extends JumpCheckerComponent {
  (template: TemplateApi)  => {
    template.members.foreach { member => check((member, encls)) }
  }
}

@component(tree, encls)
trait NewJumpCheckerComponent extends JumpCheckerComponent {
  (nw: NewApi)  => {
    check((nw.app, encls))
  }
}

@component(tree, encls)
trait SelectJumpCheckerComponent extends JumpCheckerComponent {
  (select: SelectApi)  => {
    check((select.qual, encls))
    check((select.tree, encls))
  }
}

@component(tree, encls)
trait ThisJumpCheckerComponent extends JumpCheckerComponent {
  (ths: ThisApi)  => ths
}

@component(tree, encls)
trait SuperJumpCheckerComponent extends JumpCheckerComponent {
  (spr: SuperApi)  => spr
}

@component(tree, encls)
trait MethodDefJumpCheckerComponent extends
    brokenj.typechecker.MethodDefJumpCheckerComponent {
  (mthd: PMethodDefApi)  => super.apply((mthd, encls))
}
