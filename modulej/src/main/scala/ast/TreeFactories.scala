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

package ch.usi.inf.l3.sana.modulej.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.ast.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast.UseTree
import sana.tiny.names.Name
import sana.tiny.modifiers.Flags
import sana.tiny.ast._
import sana.calcj.ast._
import sana.brokenj.ast._
import sana.arrayj.ast._
import sana.calcj.ast.operators.{UOp, BOp}
import sana.primj.ast.{MethodDefApi => _, ProgramApi => _, _}
import sana.ooj.ast.{MethodDefApi => _, CompilationUnitApi => _, _}
import sana.robustj.ast._
import sana.ppj.ast._
import sana.ppj.ast.{TreeFactories => TF}
import sana.primj.types._


trait TreeFactories {
  /**
   * Creates a new tree to represent an import statement
   *
   * @param qual the fully qualified imported name
   * @param isOnDemand is this import an on-demand statement
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def mkImport(qual: UseTree, isOnDemand: Boolean,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ImportApi = {
    val res = new Import(qual, isOnDemand)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res
  }

  /**
   * Creates a new tree to represent a compilation unit
   *
   * @param imports the list of all the imported statements of this
   *                compilation unit
   * @param module the package tree in this compilation unit
   * @param sourceName the name of the source file of this compilation unit
   * @param sourcePath the path of the source file of this compilation unit
   * @param symbol the symbol of this tree
   */
  def mkCompilationUnit(imports: List[ImportApi],
    module: PackageDefApi, sourceName: String,
    sourcePath: List[String],
    symbol: Option[Symbol] = None): CompilationUnitApi = {
    val res = new CompilationUnit(imports, module, sourceName, sourcePath)
    symbol.foreach(res.symbol = _)
    res
  }

  // ppj
  /** @see {{{sana.ppj.ast.TreeFactories.mkSynchronized}}} */
  def mkSynchronized(expr: Expr, block: BlockApi,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): SynchronizedApi = {
    sana.ppj.ast.TreeFactories.mkSynchronized(expr, block, pos, owner)
  }

  // robustj
  /** @see [[sana.ppj.ast.TreeFactories.mkThrow]] */
  def mkThrow(expr: Expr,
      pos: Option[Position] = None,
      owner: Option[Symbol] = None): ThrowApi = {
    sana.ppj.ast.TreeFactories.mkThrow(expr, pos, owner)
  }

  /** @see [[sana.ppj.ast.TreeFactories.mkTry]] */
  def mkTry(tryClause: BlockApi,
      catches: List[CatchApi], finallyClause: Option[BlockApi],
      pos: Option[Position] = None,
      owner: Option[Symbol] = None): TryApi = {
    sana.ppj.ast.TreeFactories.mkTry(tryClause, catches, finallyClause, pos, owner)
  }

  /** @see [[sana.ppj.ast.TreeFactories.mkCatch]] */
  def mkCatch(eparam: ValDefApi, catchClause: BlockApi,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): CatchApi = {
    sana.ppj.ast.TreeFactories.mkCatch(eparam, catchClause, pos, symbol, owner)
  }

  /** @see [[sana.ppj.ast.TreeFactories.mkMethodDef]] */
  def mkMethodDef(mods: Flags, ret: UseTree,
    name: Name, params: List[ValDefApi],
    throwsClause: List[UseTree],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): MethodDefApi = {
    sana.ppj.ast.TreeFactories.mkMethodDef(mods, ret, name, params, throwsClause, body, pos, symbol)
  }


  // Delegating already working ones to arrooj.ast.TreeFactories

  /** @see [[sana.ppj.ast.TreeFactories.mkProgram]] */
  def mkProgram(members: List[Tree]): ProgramApi =
    sana.ppj.ast.TreeFactories.mkProgram(members)


  /** @see [[sana.ppj.ast.TreeFactories.mkPackageDef]] */
  def mkPackageDef(containingPackages: List[Name],
    name: Name, members: List[Tree],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None): PackageDefApi =
      sana.ppj.ast.TreeFactories.mkPackageDef(containingPackages, name, members, pos, symbol)

  /** @see [[sana.ppj.ast.TreeFactories.mkClassDef]] */
  def mkClassDef(mods: Flags, name: Name,
      parents: List[UseTree], body: TemplateApi,
      pos: Option[Position] = None,
      symbol: Option[Symbol] = None,
      tpe: Option[Type] = None): ClassDefApi =
    sana.ppj.ast.TreeFactories.mkClassDef(mods, name, parents, body, pos, symbol, tpe)


  /** @see [[sana.ppj.ast.TreeFactories.mkTemplate]] */
  def mkTemplate(members: List[Tree],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): TemplateApi =
      sana.ppj.ast.TreeFactories.mkTemplate(members, pos, owner)


  /** @see [[sana.ppj.ast.TreeFactories.mkNew]] */
  def mkNew(app: ApplyApi,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): NewApi =
      sana.ppj.ast.TreeFactories.mkNew(app, pos, owner)


  /** @see [[sana.ppj.ast.TreeFactories.mkSelect]] */
  def mkSelect(qual: Tree, tree: SimpleUseTree,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): SelectApi =
      sana.ppj.ast.TreeFactories.mkSelect(qual, tree, pos, symbol, owner)

  /** @see [[sana.ppj.ast.TreeFactories.mkThis]] */
  def mkThis(pos: Option[Position] = None,
      enclosingClassSymbol: Option[Symbol] = None,
      owner: Option[Symbol] = None): ThisApi =
        sana.ppj.ast.TreeFactories.mkThis(pos, enclosingClassSymbol, owner)

  /** @see [[sana.ppj.ast.TreeFactories.mkSuper]] */
  def mkSuper(pos: Option[Position] = None,
      enclosingClassSymbol: Option[Symbol] = None,
      owner: Option[Symbol] = None): SuperApi =
        sana.ppj.ast.TreeFactories.mkSuper(pos, enclosingClassSymbol, owner)



  /** @see [[sana.ppj.ast.TreeFactories.mkIdent]] */
  def mkIdent(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            enclosing: Option[Symbol]  = None,
            owner: Option[Symbol] = None): IdentApi =
    sana.ppj.ast.TreeFactories.mkIdent(name, pos, symbol, enclosing, owner)

  /** @see [[sana.ppj.ast.TreeFactories.mkTypeUse]] */
  def mkTypeUse(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            enclosing: Option[Symbol]  = None,
            owner: Option[Symbol] = None): TypeUseApi =
    sana.ppj.ast.TreeFactories.mkTypeUse(name, pos, symbol, enclosing, owner)

  /** @see [[sana.ppj.ast.TreeFactories.mkArrayInitializer]] */
  def mkArrayInitializer(elements: List[Expr],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayInitializerApi =
    sana.ppj.ast.TreeFactories.mkArrayInitializer(elements, pos, symbol, owner, tpe)


  /** @see [[sana.ppj.ast.TreeFactories.mkArrayAccess]] */
  def mkArrayAccess(array: Expr, index: Expr,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayAccessApi =
    sana.ppj.ast.TreeFactories.mkArrayAccess(array, index, pos, symbol, owner, tpe)

  /** @see [[sana.ppj.ast.TreeFactories.mkArrayTypeUse]] */
  def mkArrayTypeUse(tpt: UseTree,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayTypeUseApi =
    sana.ppj.ast.TreeFactories.mkArrayTypeUse(tpt, pos, symbol, owner, tpe)

  /** @see [[sana.ppj.ast.TreeFactories.mkArrayCreation]] */
  def mkArrayCreation(array: Expr,
    size: Option[Expr],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayCreationApi =
    sana.ppj.ast.TreeFactories.mkArrayCreation(array, size, pos, symbol, owner, tpe)



  // From calcj
  /** @see [[sana.ppj.ast.TreeFactories.mkCast]] */
  def mkCast(tpt: UseTree, expr: Expr,
           pos: Option[Position] = None): CastApi = {
    sana.ppj.ast.TreeFactories.mkCast(tpt, expr, pos)
  }


  /** @see [[sana.ppj.ast.TreeFactories.mkLiteral]] */
  def mkLiteral(constant: Constant,
              pos: Option[Position] = None,
              owner: Option[Symbol] = None): LiteralApi = {
    sana.ppj.ast.TreeFactories.mkLiteral(constant, pos, owner)
  }


  /** @see [[sana.ppj.ast.TreeFactories.mkBinary]] */
  def mkBinary(lhs: Expr, op: BOp, rhs: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): BinaryApi = {
    sana.ppj.ast.TreeFactories.mkBinary(lhs, op, rhs, pos, tpe, owner)
  }

  /** @see [[sana.ppj.ast.TreeFactories.mkUnary]] */
  def mkUnary(isPostfix: Boolean, op: UOp, expr: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): UnaryApi = {
    sana.ppj.ast.TreeFactories.mkUnary(isPostfix, op, expr, pos, tpe, owner)
  }

  // From primj


  /** @see [[sana.ppj.ast.TreeFactories.mkAssign]] */
  def mkAssign(lhs: Expr, rhs: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): AssignApi = {
    sana.ppj.ast.TreeFactories.mkAssign(lhs, rhs, pos, owner)
  }


  /** @see [[sana.ppj.ast.TreeFactories.mkIf]] */
  def mkIf(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): IfApi = {
    sana.ppj.ast.TreeFactories.mkIf(cond, thenp, elsep, pos, owner)
  }


  /** @see [[sana.ppj.ast.TreeFactories.mkWhile]] */
  def mkWhile(isDoWhile: Boolean, cond: Expr, body: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): WhileApi = {
    sana.ppj.ast.TreeFactories.mkWhile(isDoWhile, cond, body, pos, owner)
  }

  /** @see [[sana.ppj.ast.TreeFactories.mkFor]] */
  def mkFor(inits: List[Tree], cond: Expr, steps: List[Expr],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ForApi = {
    sana.ppj.ast.TreeFactories.mkFor(inits, cond, steps, body, pos, symbol)
  }

  /** @see [[sana.ppj.ast.TreeFactories.mkBlock]] */
  def mkBlock(stmts: List[Tree],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None): BlockApi = {
    sana.ppj.ast.TreeFactories.mkBlock(stmts, pos, symbol)
  }

  /** @see [[sana.ppj.ast.TreeFactories.mkTernary]] */
  def mkTernary(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    tpe: Option[Type]     = None,
    owner: Option[Symbol] = None): TernaryApi = {
    sana.ppj.ast.TreeFactories.mkTernary(cond, thenp, elsep, pos, tpe, owner)
  }


  /** @see [[sana.ppj.ast.TreeFactories.mkApply]] */
  def mkApply(fun: Expr, args: List[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ApplyApi = {
    sana.ppj.ast.TreeFactories.mkApply(fun, args, pos, owner)
  }

  /** @see [[sana.ppj.ast.TreeFactories.mkReturn]] */
  def mkReturn(expr: Option[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ReturnApi = {
    sana.ppj.ast.TreeFactories.mkReturn(expr, pos, owner)
  }



  /** @see [[sana.ppj.ast.TreeFactories.mkValDef]] */
  def mkValDef(mods: Flags, tpt: UseTree, name: Name,
    rhs: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ValDefApi = {

    sana.ppj.ast.TreeFactories.mkValDef(mods, tpt, name, rhs, pos, symbol)
  }

  // brokenj
  /** @see [[sana.ppj.ast.TreeFactories.mkLabel]] */
  def mkLabel(name: Name, stmt: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): LabelApi = {
    sana.ppj.ast.TreeFactories.mkLabel(name, stmt, pos, owner)
  }

  /** @see [[sana.ppj.ast.TreeFactories.mkBreak]] */
  def mkBreak(label: Option[Name],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): BreakApi = {
    sana.ppj.ast.TreeFactories.mkBreak(label, pos, owner)
  }

  /** @see [[sana.ppj.ast.TreeFactories.mkContinue]] */
  def mkContinue(label: Option[Name],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ContinueApi = {
    sana.ppj.ast.TreeFactories.mkContinue(label, pos, owner)
  }

  /** @see [[sana.ppj.ast.TreeFactories.mkCase]] */
  def mkCase(guards: List[Expr], body: Tree,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): CaseApi = {
    sana.ppj.ast.TreeFactories.mkCase(guards, body, pos, owner)
  }

  /** @see [[sana.ppj.ast.TreeFactories.mkSwitch]] */
  def mkSwitch(expr: Expr, cases: List[CaseApi],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): SwitchApi = {
    sana.ppj.ast.TreeFactories.mkSwitch(expr, cases, pos, owner)
  }
}

object TreeFactories extends TreeFactories
