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
import sana.primj.types._



trait TreeFactories extends sana.calcj.ast.TreeFactories {

  /**
   * Creates a new tree for program
   *
   * @param members the list of definitions of this program
   * @param sourceName the name of the source file of this tree
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   */
  def mkProgram(members: List[DefTree], sourceName: String,
              symbol: Option[Symbol] = None): ProgramApi = {
    val res = new Program(members, sourceName)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    res
  }

  /**
   * Creates a new tree for assignment
   *
   * @param lhs the left-hand side expression
   * @param rhs the right-hand side expression
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   * @param owner the owner of this tree
   */
  def mkAssign(lhs: Expr, rhs: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): AssignApi = {
    val res = new Assign(lhs, rhs)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    lhs.tpe.foreach(res.tpe = _)
    res
  }

  /**
   * Creates a new tree for if-else statement
   *
   * @param cond the condition of if-else
   * @param thenp the then-clause of if-else
   * @param elsep the else-clause of if-else
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def mkIf(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): IfApi = {
    val res = new If(cond, thenp, elsep)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res.tpe = VoidType
    res
  }

  /**
   * Creates a new tree for while/do-while statement
   *
   * @param isDoWhile A flag to indicate if this loop is while or do-while loop
   * @param cond the condition of the loop
   * @param body the body of the loop
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def mkWhile(isDoWhile: Boolean, cond: Expr, body: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): WhileApi = {
    val res = new While(isDoWhile, cond, body)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res.tpe = VoidType
    res
  }

  /**
   * Creates a new tree for for-loop statement
   *
   * @param cond the condition of the loop
   * @param inits the initialization statements of the loop
   * @param steps the steps expressions of the loop
   * @param body the body of the loop
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def mkFor(inits: List[Tree], cond: Expr, steps: List[Expr],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ForApi = {
    val res = new For(inits, cond, steps, body)
    pos.foreach(res.pos = _)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    res.tpe = VoidType
    res
  }

  /**
   * Creates a new block of statements
   *
   * @param sttmts the statements of the block
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   */
  def mkBlock(stmts: List[Tree],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None): BlockApi = {
    val res = new Block(stmts)
    pos.foreach(res.pos = _)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    stmts match {
      case Nil    =>
        res.tpe = VoidType
      case _      =>
        stmts.last.tpe.foreach(res.tpe = _)
    }
    res
  }

  /**
   * Creates a new ternary expression
   *
   * @param cond the condition of this tree
   * @param thenp the then-clause of the tree
   * @param elsep the else-clause of the tree
   * @param pos the position of this tree
   * @param tpe the type-information of this tree
   * @param owner the owner of this tree
   */
  def mkTernary(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    tpe: Option[Type]     = None,
    owner: Option[Symbol] = None): TernaryApi = {
    val res = new Ternary(cond, thenp, elsep)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    tpe.foreach(res.tpe = _)
    res
  }

  /**
   * Creates a new function/method application expression
   *
   * @param fun the function/method to be applied
   * @param args the arguments of this application
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def mkApply(fun: Expr, args: List[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ApplyApi = {

    val res = new Apply(fun, args)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    fun.tpe match {
      case Some(MethodType(r, _)) =>
        res.tpe = r
      case _                      =>
        ()
    }
    res
  }

  /**
   * Creates a new tree for return statements
   *
   * @param expr the expression of this return statement
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def mkReturn(expr: Option[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ReturnApi = {
    val res = new Return(expr)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    expr.flatMap(_.tpe).foreach(res.tpe = _)
    res
  }

  /**
   * Creates a new tree for method definitions
   *
   * @param ret the type-tree for this method's return type
   * @param name the name of this method
   * @param params the list of parameters of this method
   * @param body the body of this method
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   */
  def mkMethodDef(ret: UseTree,
    name: Name, params: List[ValDefApi],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): MethodDefApi = {
    val res = new MethodDef(ret, name, params, body)
    pos.foreach(res.pos = _)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    val tys = params.flatMap(_.tpe)
    ret.tpe.foreach(t => res.tpe = MethodType(t, tys))
    res
  }

  /**
   * Creates a new tree for variable definitions
   *
   * @param mods the modifiers (flags) of this variable
   * @param tpt the type-tree for this variable's type
   * @param name the name of this variable
   * @param body the right-hand side expression of this variable
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   */
  def mkValDef(mods: Flags, tpt: UseTree, name: Name,
    rhs: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ValDefApi = {

    val res = new ValDef(mods, tpt, name, rhs)
    pos.foreach(res.pos = _)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    tpt.tpe.foreach(res.tpe = _)
    res
  }

}

object TreeFactories extends TreeFactories
