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
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.tiny.modifiers.Flags
import sana.tiny.ast._
import sana.calcj.ast._
import sana.brokenj.ast._
import sana.arrayj.ast._
import sana.calcj.ast.operators.{UOp, BOp}
import sana.primj.ast.{MethodDefApi => _, ProgramApi => _, _}
import sana.ooj.ast.{MethodDefApi => _, _}
import sana.arrooj.ast.{TreeFactories => TF}
import sana.primj.types._
import sana.arrooj.ast.Implicits._
// import sana.ooj.ast.TreeExtractors._

trait TreeFactories {

  /**
   * Creates a new tree to represent a `throw` statement
   *
   * @param expr the expression of this statement
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def mkThrow(expr: Expr,
      pos: Option[Position] = None,
      owner: Option[Symbol] = None): ThrowApi = {

    val res = new Throw(expr)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res
  }

  /**
   * Creates a new tree to represent a `try-catch` statement
   *
   * @param tryClause the try-clause of this statement
   * @param catches the list of catches of this statement
   * @param finallyClause the finally-clause of this statement
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def mkTry(tryClause: BlockApi,
      catches: List[CatchApi], finallyClause: Option[BlockApi],
      pos: Option[Position] = None,
      owner: Option[Symbol] = None): TryApi = {
    val res = new Try(tryClause, catches, finallyClause)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res
  }

  /**
   * Creates a new tree to represent a `catch` statement
   *
   * @param eparam the exception parameter of this statement
   * @param catchClause the body of this catch statement
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   * @param owner the owner of this tree
   */
  def mkCatch(eparam: ValDefApi, catchClause: BlockApi,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): CatchApi = {
    val res = new Catch(eparam, catchClause)
    pos.foreach(res.pos = _)
    symbol.foreach(res.symbol = _)
    owner.foreach(res.owner = _)
    res
  }

  /**
   * Creates a new tree to represent a method definition
   *
   * @param mods the modifiers of this method
   * @param ret the type-tree of the return type of this method
   * @param name the name of this method
   * @param params the list of the parameters of this method
   * @param throwsClause the list of declared thrown exceptions of this method/function
   * @param body the body of this method
   * @param pos the position of this tree
   * @param symbol the symbol of this tree
   */
  def mkMethodDef(mods: Flags, ret: UseTree,
    name: Name, params: List[ValDefApi],
    throwsClause: List[UseTree],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): MethodDefApi = {
    val res = new MethodDef(mods, ret, name, params, throwsClause, body)
    pos.foreach(res.pos = _)
    symbol.foreach( sym => {
      res.symbol = sym
      sym.owner.foreach(res.owner = _)
    })
    val tys = params.flatMap(_.tpe)
    ret.tpe.foreach(t => res.tpe = MethodType(t, tys))
    res
  }


  // Delegating already working ones to arrooj.ast.TreeFactories

  /** @see [[sana.arrooj.ast.TreeFactories.mkProgram]] */
  def mkProgram(members: List[Tree]): ProgramApi =
    TF.mkProgram(members)

  /** @see [[sana.arrooj.ast.TreeFactories.mkCompilationUnit]] */
  def mkCompilationUnit(module: PackageDefApi, sourceName: String,
    sourcePath: List[String],
    symbol: Option[Symbol] = None): CompilationUnitApi =
      TF.mkCompilationUnit(module, sourceName, sourcePath, symbol)

  /** @see [[sana.arrooj.ast.TreeFactories.mkPackageDef]] */
  def mkPackageDef(containingPackages: List[Name],
    name: Name, members: List[Tree],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None): PackageDefApi =
      TF.mkPackageDef(containingPackages, name, members, pos, symbol)

  /** @see [[sana.arrooj.ast.TreeFactories.mkClassDef]] */
  def mkClassDef(mods: Flags, name: Name,
      parents: List[UseTree], body: TemplateApi,
      pos: Option[Position] = None,
      symbol: Option[Symbol] = None,
      tpe: Option[Type] = None): ClassDefApi =
    TF.mkClassDef(mods, name, parents, body, pos, symbol, tpe)


  /** @see [[sana.arrooj.ast.TreeFactories.mkTemplate]] */
  def mkTemplate(members: List[Tree],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): TemplateApi =
      TF.mkTemplate(members, pos, owner)


  /** @see [[sana.arrooj.ast.TreeFactories.mkNew]] */
  def mkNew(app: ApplyApi,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): NewApi =
      TF.mkNew(app, pos, owner)


  /** @see [[sana.arrooj.ast.TreeFactories.mkSelect]] */
  def mkSelect(qual: Tree, tree: SimpleUseTree,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): SelectApi =
      TF.mkSelect(qual, tree, pos, symbol, owner)

  /** @see [[sana.arrooj.ast.TreeFactories.mkThis]] */
  def mkThis(pos: Option[Position] = None,
      enclosingClassSymbol: Option[Symbol] = None,
      owner: Option[Symbol] = None): ThisApi =
        TF.mkThis(pos, enclosingClassSymbol, owner)

  /** @see [[sana.arrooj.ast.TreeFactories.mkSuper]] */
  def mkSuper(pos: Option[Position] = None,
      enclosingClassSymbol: Option[Symbol] = None,
      owner: Option[Symbol] = None): SuperApi =
        TF.mkSuper(pos, enclosingClassSymbol, owner)



  /** @see [[sana.arrooj.ast.TreeFactories.mkIdent]] */
  def mkIdent(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            enclosing: Option[Symbol]  = None,
            owner: Option[Symbol] = None): IdentApi =
    TF.mkIdent(name, pos, symbol, enclosing, owner)

  /** @see [[sana.arrooj.ast.TreeFactories.mkTypeUse]] */
  def mkTypeUse(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            enclosing: Option[Symbol]  = None,
            owner: Option[Symbol] = None): TypeUseApi =
    TF.mkTypeUse(name, pos, symbol, enclosing, owner)

  /** @see {{{sana.arrooj.ast.TreeFactories.mkArrayInitializer}}} */
  def mkArrayInitializer(elements: List[Expr],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayInitializerApi =
    TF.mkArrayInitializer(elements, pos, symbol, owner, tpe)


  /** @see {{{sana.arrooj.ast.TreeFactories.mkArrayAccess}}} */
  def mkArrayAccess(array: Expr, index: Expr,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayAccessApi =
    TF.mkArrayAccess(array, index, pos, symbol, owner, tpe)

  /** @see {{{sana.arrooj.ast.TreeFactories.mkArrayTypeUse}}} */
  def mkArrayTypeUse(tpt: UseTree,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayTypeUseApi =
    TF.mkArrayTypeUse(tpt, pos, symbol, owner, tpe)

  /** @see {{{sana.arrooj.ast.TreeFactories.mkArrayCreation}}} */
  def mkArrayCreation(array: Expr,
    size: Option[Expr],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayCreationApi =
    TF.mkArrayCreation(array, size, pos, symbol, owner, tpe)



  // From calcj
  /** @see [[sana.arrooj.ast.TreeFactories.mkCast]] */
  def mkCast(tpt: UseTree, expr: Expr,
           pos: Option[Position] = None): CastApi = {
    TF.mkCast(tpt, expr, pos)
  }


  /** @see [[sana.arrooj.ast.TreeFactories.mkLiteral]] */
  def mkLiteral(constant: Constant,
              pos: Option[Position] = None,
              owner: Option[Symbol] = None): LiteralApi = {
    TF.mkLiteral(constant, pos, owner)
  }


  /** @see [[sana.arrooj.ast.TreeFactories.mkBinary]] */
  def mkBinary(lhs: Expr, op: BOp, rhs: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): BinaryApi = {
    TF.mkBinary(lhs, op, rhs, pos, tpe, owner)
  }

  /** @see [[sana.arrooj.ast.TreeFactories.mkUnary]] */
  def mkUnary(isPostfix: Boolean, op: UOp, expr: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): UnaryApi = {
    TF.mkUnary(isPostfix, op, expr, pos, tpe, owner)
  }

  // From primj


  /** @see [[sana.arrooj.ast.TreeFactories.mkAssign]] */
  def mkAssign(lhs: Expr, rhs: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): AssignApi = {
    TF.mkAssign(lhs, rhs, pos, owner)
  }


  /** @see [[sana.arrooj.ast.TreeFactories.mkIf]] */
  def mkIf(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): IfApi = {
    TF.mkIf(cond, thenp, elsep, pos, owner)
  }


  /** @see [[sana.arrooj.ast.TreeFactories.mkWhile]] */
  def mkWhile(isDoWhile: Boolean, cond: Expr, body: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): WhileApi = {
    TF.mkWhile(isDoWhile, cond, body, pos, owner)
  }

  /** @see [[sana.arrooj.ast.TreeFactories.mkFor]] */
  def mkFor(inits: List[Tree], cond: Expr, steps: List[Expr],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ForApi = {
    TF.mkFor(inits, cond, steps, body, pos, symbol)
  }

  /** @see [[sana.arrooj.ast.TreeFactories.mkBlock]] */
  def mkBlock(stmts: List[Tree],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None): BlockApi = {
    TF.mkBlock(stmts, pos, symbol)
  }

  /** @see [[sana.arrooj.ast.TreeFactories.mkTernary]] */
  def mkTernary(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    tpe: Option[Type]     = None,
    owner: Option[Symbol] = None): TernaryApi = {
    TF.mkTernary(cond, thenp, elsep, pos, tpe, owner)
  }


  /** @see [[sana.arrooj.ast.TreeFactories.mkApply]] */
  def mkApply(fun: Expr, args: List[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ApplyApi = {
    TF.mkApply(fun, args, pos, owner)
  }

  /** @see [[sana.arrooj.ast.TreeFactories.mkReturn]] */
  def mkReturn(expr: Option[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ReturnApi = {
    TF.mkReturn(expr, pos, owner)
  }



  /** @see [[sana.arrooj.ast.TreeFactories.mkValDef]] */
  def mkValDef(mods: Flags, tpt: UseTree, name: Name,
    rhs: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ValDefApi = {

    TF.mkValDef(mods, tpt, name, rhs, pos, symbol)
  }

  // brokenj
  /** @see [[sana.arrooj.ast.TreeFactories.mkLabel]] */
  def mkLabel(name: Name, stmt: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): LabelApi = {
    TF.mkLabel(name, stmt, pos, owner)
  }

  /** @see [[sana.arrooj.ast.TreeFactories.mkBreak]] */
  def mkBreak(label: Option[Name],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): BreakApi = {
    TF.mkBreak(label, pos, owner)
  }

  /** @see [[sana.arrooj.ast.TreeFactories.mkContinue]] */
  def mkContinue(label: Option[Name],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ContinueApi = {
    TF.mkContinue(label, pos, owner)
  }

  /** @see [[sana.arrooj.ast.TreeFactories.mkCase]] */
  def mkCase(guards: List[Expr], body: Tree,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): CaseApi = {
    TF.mkCase(guards, body, pos, owner)
  }

  /** @see [[sana.arrooj.ast.TreeFactories.mkSwitch]] */
  def mkSwitch(expr: Expr, cases: List[CaseApi],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): SwitchApi = {
    TF.mkSwitch(expr, cases, pos, owner)
  }
}

object TreeFactories extends TreeFactories
