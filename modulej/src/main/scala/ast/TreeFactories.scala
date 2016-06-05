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
  def mkImport(qual: UseTree, isOnDemand: Boolean,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ImportApi = {
    val res = new Import(qual, isOnDemand)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res
  }

  def mkCompilationUnit(imports: List[ImportApi],
    module: PackageDefApi, sourceName: String,
    sourcePath: List[String],
    symbol: Option[Symbol] = None): CompilationUnitApi = {
    val res = new CompilationUnit(imports, module, sourceName, sourcePath)
    symbol.foreach(res.symbol = _)
    res
  }

  // ppj
  def mkSynchronized(expr: Expr, block: BlockApi,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): SynchronizedApi = {
    TF.mkSynchronized(expr, block, pos, owner)
  }

  // robustj
  def mkThrow(expr: Expr,
      pos: Option[Position] = None,
      owner: Option[Symbol] = None): ThrowApi = {
    TF.mkThrow(expr, pos, owner)
  }

  def mkTry(tryClause: BlockApi,
      catches: List[CatchApi], finallyClause: Option[BlockApi],
      pos: Option[Position] = None,
      owner: Option[Symbol] = None): TryApi = {
    TF.mkTry(tryClause, catches, finallyClause, pos, owner)
  }

  def mkCatch(eparam: ValDefApi, catchClause: BlockApi,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): CatchApi = {
    TF.mkCatch(eparam, catchClause, pos, symbol, owner)
  }

  def mkMethodDef(mods: Flags, ret: UseTree,
    name: Name, params: List[ValDefApi],
    throwsClause: List[UseTree],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): MethodDefApi = {
    TF.mkMethodDef(mods, ret, name, params, throwsClause, body, pos, symbol)
  }


  // Delegating already working ones to arrooj.ast.TreeFactories

  def mkProgram(members: List[Tree]): ProgramApi =
    TF.mkProgram(members)


  def mkPackageDef(containingPackages: List[Name],
    name: Name, members: List[Tree],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None): PackageDefApi =
      TF.mkPackageDef(containingPackages, name, members, pos, symbol)

  def mkClassDef(mods: Flags, name: Name,
      parents: List[UseTree], body: TemplateApi,
      pos: Option[Position] = None,
      symbol: Option[Symbol] = None,
      tpe: Option[Type] = None): ClassDefApi =
    TF.mkClassDef(mods, name, parents, body, pos, symbol, tpe)


  def mkTemplate(members: List[Tree],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): TemplateApi =
      TF.mkTemplate(members, pos, owner)


  def mkNew(app: ApplyApi,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): NewApi =
      TF.mkNew(app, pos, owner)


  def mkSelect(qual: Tree, tree: SimpleUseTree,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None): SelectApi =
      TF.mkSelect(qual, tree, pos, symbol, owner)

  def mkThis(pos: Option[Position] = None,
      enclosingClassSymbol: Option[Symbol] = None,
      owner: Option[Symbol] = None): ThisApi =
        TF.mkThis(pos, enclosingClassSymbol, owner)

  def mkSuper(pos: Option[Position] = None,
      enclosingClassSymbol: Option[Symbol] = None,
      owner: Option[Symbol] = None): SuperApi =
        TF.mkSuper(pos, enclosingClassSymbol, owner)



  def mkIdent(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            enclosing: Option[Symbol]  = None,
            owner: Option[Symbol] = None): IdentApi =
    TF.mkIdent(name, pos, symbol, enclosing, owner)

  def mkTypeUse(name: Name,
            pos: Option[Position] = None,
            symbol: Option[Symbol] = None,
            enclosing: Option[Symbol]  = None,
            owner: Option[Symbol] = None): TypeUseApi =
    TF.mkTypeUse(name, pos, symbol, enclosing, owner)

  def mkArrayInitializer(elements: List[Expr],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayInitializerApi =
    TF.mkArrayInitializer(elements, pos, symbol, owner, tpe)


  def mkArrayAccess(array: Expr, index: Expr,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayAccessApi =
    TF.mkArrayAccess(array, index, pos, symbol, owner, tpe)

  def mkArrayTypeUse(tpt: UseTree,
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayTypeUseApi =
    TF.mkArrayTypeUse(tpt, pos, symbol, owner, tpe)

  def mkArrayCreation(array: Expr,
    size: Option[Expr],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None,
    owner: Option[Symbol] = None,
    tpe: Option[Type] = None): ArrayCreationApi =
    TF.mkArrayCreation(array, size, pos, symbol, owner, tpe)



  // From calcj
  def mkCast(tpt: UseTree, expr: Expr,
           pos: Option[Position] = None): CastApi = {
    TF.mkCast(tpt, expr, pos)
  }


  def mkLiteral(constant: Constant,
              pos: Option[Position] = None,
              owner: Option[Symbol] = None): LiteralApi = {
    TF.mkLiteral(constant, pos, owner)
  }


  def mkBinary(lhs: Expr, op: BOp, rhs: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): BinaryApi = {
    TF.mkBinary(lhs, op, rhs, pos, tpe, owner)
  }

  def mkUnary(isPostfix: Boolean, op: UOp, expr: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): UnaryApi = {
    TF.mkUnary(isPostfix, op, expr, pos, tpe, owner)
  }

  // From primj


  def mkAssign(lhs: Expr, rhs: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): AssignApi = {
    TF.mkAssign(lhs, rhs, pos, owner)
  }


  def mkIf(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): IfApi = {
    TF.mkIf(cond, thenp, elsep, pos, owner)
  }


  def mkWhile(isDoWhile: Boolean, cond: Expr, body: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): WhileApi = {
    TF.mkWhile(isDoWhile, cond, body, pos, owner)
  }

  def mkFor(inits: List[Tree], cond: Expr, steps: List[Expr],
    body: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ForApi = {
    TF.mkFor(inits, cond, steps, body, pos, symbol)
  }

  def mkBlock(stmts: List[Tree],
    pos: Option[Position] = None,
    symbol: Option[Symbol] = None): BlockApi = {
    TF.mkBlock(stmts, pos, symbol)
  }

  def mkTernary(cond: Expr, thenp: Expr, elsep: Expr,
    pos: Option[Position] = None,
    tpe: Option[Type]     = None,
    owner: Option[Symbol] = None): TernaryApi = {
    TF.mkTernary(cond, thenp, elsep, pos, tpe, owner)
  }


  def mkApply(fun: Expr, args: List[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ApplyApi = {
    TF.mkApply(fun, args, pos, owner)
  }

  def mkReturn(expr: Option[Expr],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ReturnApi = {
    TF.mkReturn(expr, pos, owner)
  }



  def mkValDef(mods: Flags, tpt: UseTree, name: Name,
    rhs: Expr, pos: Option[Position] = None,
    symbol: Option[Symbol] = None): ValDefApi = {

    TF.mkValDef(mods, tpt, name, rhs, pos, symbol)
  }

  // brokenj
  def mkLabel(name: Name, stmt: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): LabelApi = {
    TF.mkLabel(name, stmt, pos, owner)
  }

  def mkBreak(label: Option[Name],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): BreakApi = {
    TF.mkBreak(label, pos, owner)
  }

  def mkContinue(label: Option[Name],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ContinueApi = {
    TF.mkContinue(label, pos, owner)
  }

  def mkCase(guards: List[Expr], body: Tree,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): CaseApi = {
    TF.mkCase(guards, body, pos, owner)
  }

  def mkSwitch(expr: Expr, cases: List[CaseApi],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): SwitchApi = {
    TF.mkSwitch(expr, cases, pos, owner)
  }
}

object TreeFactories extends TreeFactories
