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
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.primj.ast.Implicits._
import sana.tiny.modifiers.Flags
import sana.tiny.ast._
import sana.calcj.ast._
import sana.primj.ast._



trait TreeCopiers extends sana.primj.ast.TreeCopiers {

  /**
   * Returns a copy of a label statement
   *
   * @param template the tree to be copied
   * @param name the name of this label statement
   * @param stmt the statement of this label statement
   */
  def copyLabel(template: LabelApi)(name: Name = template.name,
    stmt: Expr = template.stmt): LabelApi = {
    val res = TreeFactories.mkLabel(name, stmt)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a break statement
   *
   * @param template the tree to be copied
   * @param label the label of this break statement
   */
  def copyBreak(template: BreakApi)(label: Option[Name] =
    template.label): BreakApi = {
    val res = TreeFactories.mkBreak(label)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a continue statement
   *
   * @param template the tree to be copied
   * @param label the label of this continue statement
   */
  def copyContinue(template: ContinueApi)(label: Option[Name] =
        template.label): ContinueApi = {
    val res = TreeFactories.mkContinue(label)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a case
   *
   * @param template the tree to be copied
   * @param guards the guards of this case
   * @param body the body of this tree
   */
  def copyCase(template: CaseApi)(guards: List[Expr] = template.guards,
    body: Tree = template.body): CaseApi = {
    val res = TreeFactories.mkCase(guards, body)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a switch-statement
   *
   * @param template the tree to be copied
   * @param expr the expression of this switch statement
   * @param cases the cases of this switch statement
   */
  def copySwitch(template: SwitchApi)(expr: Expr = template.expr,
    cases: List[CaseApi] = template.cases): SwitchApi = {
    val res = TreeFactories.mkSwitch(expr, cases)
    copyProperties(template, res)
    res
  }

}

object TreeCopiers extends TreeCopiers
