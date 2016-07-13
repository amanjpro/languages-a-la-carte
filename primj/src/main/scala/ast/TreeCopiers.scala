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

package ch.usi.inf.l3.sana.primj.ast

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



trait TreeCopiers extends sana.calcj.ast.TreeCopiers {
  /**
   * Returns a copy of a program tree
   *
   * @param template the tree to be copied
   * @param members the list of the program's definitions
   * @param sourceName the name of the source file of this program
   */
  def copyProgram(template: ProgramApi)(members: List[DefTree] =
    template.members,
    sourceName: String = template.sourceName): ProgramApi = {
    val res = TreeFactories.mkProgram(members, sourceName)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of an assign tree
   *
   * @param template the tree to be copied
   * @param lhs the left-hand side of this assignment tree
   * @param rhs the right-hand side of this assignment tree
   */
  def copyAssign(template: AssignApi)(lhs: Expr = template.lhs,
    rhs: Expr = template.rhs): AssignApi = {
    val res = TreeFactories.mkAssign(lhs, rhs)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of if-else tree
   *
   * @param template the tree to be copied
   * @param cond the condition of this if-else tree
   * @param thenp the then-clause of this if-else tree
   * @param elsep the else-clause of this if-else tree
   */
  def copyIf(template: IfApi)(cond: Expr = template.cond,
    thenp: Expr = template.thenp, elsep: Expr = template.elsep): IfApi = {
    val res = TreeFactories.mkIf(cond, thenp, elsep)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of do/while-loop tree
   *
   * @param template the tree to be copied
   * @param isDoWhile a flag to indicate if this tree is a while or do-while
   *                  loop
   * @param cond the condition of this loop
   * @param body the body of this loop
   */
  def copyWhile(template: WhileApi)(isDoWhile: Boolean = template.isDoWhile,
    cond: Expr = template.cond, body: Expr = template.body): WhileApi = {
    val res = TreeFactories.mkWhile(isDoWhile, cond, body)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of for-loop tree
   *
   * @param template the tree to be copied
   * @param inits the initialization statements of this loop
   * @param cond the condition of this loop
   * @param steps the step statements of this loop
   * @param body the body of this loop
   */
  def copyFor(template: ForApi)(inits: List[Tree] = template.inits,
    cond: Expr = template.cond, steps: List[Expr] = template.steps,
    body: Expr = template.body): ForApi = {
    val res = TreeFactories.mkFor(inits, cond, steps, body)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of block of statements
   *
   * @param template the tree to be copied
   * @param stmts the list statements of this block
   */
  def copyBlock(template: BlockApi)(stmts: List[Tree] =
    template.stmts): BlockApi = {
    val res = TreeFactories.mkBlock(stmts)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a ternary expression
   *
   * @param template the tree to be copied
   * @param cond the condition of this tree
   * @param thenp the then-clause of this tree
   * @param elsep the else-clause of this tree
   */
  def copyTernary(template: TernaryApi)(cond: Expr = template.cond,
    thenp: Expr = template.thenp,
    elsep: Expr = template.elsep): TernaryApi = {
    val res = TreeFactories.mkTernary(cond, thenp, elsep)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a method/function application
   *
   * @param template the tree to be copied
   * @param fun the function/method to be applied
   * @param args the list arguments of this function/method application
   */
  def copyApply(template: ApplyApi)(fun: Expr = template.fun,
    args: List[Expr] = template.args): ApplyApi = {

    val res = TreeFactories.mkApply(fun, args)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a return statement
   *
   * @param template the tree to be copied
   * @param expr the expression of this return statement
   */
  def copyReturn(template: ReturnApi)(expr: Option[Expr] =
      template.expr): ReturnApi = {
    val res = TreeFactories.mkReturn(expr)
    copyProperties(template, res)
    res
  }


  /**
   * Returns a copy of a method definition
   *
   * @param template the tree to be copied
   * @param ret the return type-tree of this method/function
   * @param name the name of this method/function
   * @param params the list of parameters of this method/function
   * @param body the body of this method/function
   */
  def copyMethodDef(template: MethodDefApi)(ret: UseTree = template.ret,
    name: Name = template.name, params: List[ValDefApi]  = template.params,
    body: Expr = template.body): MethodDefApi = {
    val res = TreeFactories.mkMethodDef(ret, name, params, body)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a variable definition
   *
   * @param template the tree to be copied
   * @param mods the modifiers (i.e. flags) of this variable
   * @param tpt the type-tree of this variable
   * @param name the name of this variable
   * @param rhs the right-hand side of this variable
   */
  def copyValDef(template: ValDefApi)(mods: Flags = template.mods,
    tpt: UseTree = template.tpt, name: Name = template.name,
    rhs: Expr = template.rhs): ValDefApi = {
    val res = TreeFactories.mkValDef(mods, tpt, name, rhs)
    copyProperties(template, res)
    res
  }
}

object TreeCopiers extends TreeCopiers
