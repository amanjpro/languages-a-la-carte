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

package ch.usi.inf.l3.sana.brokenj.namers

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj

import tiny.core.TransformationComponent
import tiny.dsl._
import tiny.ast.{TreeCopiers => _, _}
import primj.ast.Implicits._
import tiny.errors.ErrorReporting.{error,warning}
import tiny.symbols._
import calcj.ast.{TreeCopiers => _, _}
import primj.ast.{TreeCopiers => _, _}
import primj.symbols._
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._
import primj.namers.SymbolAssignerComponent
import brokenj.ast._

/*
Done in Primj
Program: DONE
Assign: DONE
If: DONE
While: DONE
Block: DONE
For: DONE
Ternary: DONE
Apply: DONE
Return: DONE
MethodDef: DONE
ValDef: DONE
Ident: DONE
NoTree: DONE
TypeUse: DONE
Cast: DONE
Binary: DONE
Literal: DONE
Unary: DONE
*/

/*
Case: DONE
Swtich: DONE
Label: DONE
Break: DONE
Continue: DONE
*/


@component
trait CaseSymbolAssignerComponent extends SymbolAssignerComponent {
  (cse: CaseApi)     => {
    val owner = cse.owner
    val guards = cse.guards.map { guard =>
      owner.foreach(guard.owner = _)
      assign(guard).asInstanceOf[Expr]
    }
    owner.foreach(cse.body.owner = _)
    val body   = assign(cse.body)
    TreeCopiers.copyCase(cse)(guards = guards, body= body)
  }
}


@component
trait SwitchSymbolAssignerComponent extends SymbolAssignerComponent {
  (switch: SwitchApi)     => {
    val owner = switch.owner
    owner.foreach(switch.expr.owner = _)
    val expr  = assign(switch.expr).asInstanceOf[Expr]
    val cases = switch.cases.map { guard =>
      owner.foreach(guard.owner = _)
      assign(guard).asInstanceOf[CaseApi]
    }
    TreeCopiers.copySwitch(switch)(cases = cases, expr = expr)
  }
}


@component
trait LabelSymbolAssignerComponent extends SymbolAssignerComponent {
  (label: LabelApi)     => {
    val owner = label.owner
    owner.foreach(label.stmt.owner = _)
    val stmt  = assign(label.stmt).asInstanceOf[Expr]
    TreeCopiers.copyLabel(label)(stmt = stmt)
  }
}

@component
trait BreakSymbolAssignerComponent extends SymbolAssignerComponent {
  (break: BreakApi)     => {
    break
  }
}

@component
trait ContinueSymbolAssignerComponent extends SymbolAssignerComponent {
  (continue: ContinueApi)     => {
    continue
  }
}
