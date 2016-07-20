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
import sana.primj.ast.{MethodDefApi => _, ProgramApi => _, _}
import sana.ooj.ast.{MethodDefApi => _, _}
import sana.arrooj.ast.{TreeCopiers => TC}
import sana.calcj.ast.operators.{BOp, UOp}


trait TreeCopiers {

  /** @see [[sana.arrooj.ast.TreeCopiers.copyProperties]] */
  protected def copyProperties(template: Tree,
      newTree: Tree): Unit = newTree.attributes = template.attributes

  /**
   * Returns a copy of a "throw" tree
   *
   * @param template the tree to be copied
   * @param expr the expression of this `throw` statement
   */
  def copyThrow(template: ThrowApi)(expr: Expr = template.expr): ThrowApi = {
    val res = TreeFactories.mkThrow(expr)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a "try" tree
   *
   * @param template the tree to be copied
   * @param tryClause the `try` clause of this try-catch statement
   * @param catches the list of `catch` clauses of this try-catch statement
   * @param finallyClasue the `finally` clause of this try-catch statement
   */
  def copyTry(template: TryApi)(tryClause: BlockApi = template.tryClause,
      catches: List[CatchApi] = template.catches,
      finallyClause: Option[BlockApi] = template.finallyClause): TryApi = {
    val res = TreeFactories.mkTry(tryClause, catches, finallyClause)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a "catch" tree
   *
   * @param template the tree to be copied
   * @param eparam the exception parameter of this catch statement
   * @param catchClause the body of this "catch" tree
   */
  def copyCatch(template: CatchApi)(eparam: ValDefApi = template.eparam,
    catchClause: BlockApi = template.catchClause): CatchApi = {
    val res = TreeFactories.mkCatch(eparam, catchClause)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a method definition
   *
   * @param template the tree to be copied
   * @param mods the modifiers of this method/function
   * @param ret the return type-tree of this method/function
   * @param name the name of this method/function
   * @param params the list of parameters of this method/function
   * @param throwsClause the list of declared thrown exceptions of this method/function
   * @param body the body of this method/function
   */
  def copyMethodDef(template: MethodDefApi)(mods: Flags = template.mods,
    ret: UseTree = template.ret,
    name: Name = template.name, params: List[ValDefApi]  = template.params,
    throwsClause: List[UseTree] = template.throwsClause,
    body: Expr = template.body): MethodDefApi = {
    val res =
      TreeFactories.mkMethodDef(mods, ret, name, params, throwsClause, body)
    copyProperties(template, res)
    res
  }

  // arrooj
  /** @see {{{arrooj.ast.TreeCopiers.copyArrayInitializer}}} */
  def copyArrayInitializer(template: ArrayInitializerApi)(
      elements: List[Expr] = template.elements): ArrayInitializerApi =
    TC.copyArrayInitializer(template)(elements)


  /** @see {{{arrooj.ast.TreeCopiers.copyArrayAccess}}} */
  def copyArrayAccess(template: ArrayAccessApi)(
    array: Expr = template.array,
    index: Expr = template.index): ArrayAccessApi =
    TC.copyArrayAccess(template)(array, index)

  /** @see {{{arrooj.ast.TreeCopiers.copyArrayTypeUse}}} */
  def copyArrayTypeUse(template: ArrayTypeUseApi)(
      tpt: UseTree = template.tpt): ArrayTypeUseApi =
    TC.copyArrayTypeUse(template)(tpt)

  /** @see {{{arrooj.ast.TreeCopiers.copyArrayCreation}}} */
  def copyArrayCreation(template: ArrayCreationApi)(
      array: Expr = template.array,
      size: Option[Expr] = template.size): ArrayCreationApi =
    TC.copyArrayCreation(template)(array, size)


  // ooj

  /** @see [[arrooj.ast.TreeCopiers.copyProgram]] */
  def copyProgram(template: ProgramApi)(members: List[Tree] =
    template.members): ProgramApi = {
    TC.copyProgram(template)(members)
  }

  /** @see [[arrooj.ast.TreeCopiers.copyCompilationUnit]] */
  def copyCompilationUnit(template: CompilationUnitApi)(
    module: PackageDefApi = template.module,
    sourceName: String = template.sourceName,
    sourcePath: List[String] = template.sourcePath): CompilationUnitApi =
    TC.copyCompilationUnit(template)(module, sourceName, sourcePath)

    /** @see [[arrooj.ast.TreeCopiers.copyPackageDef]] */
  def copyPackageDef(template: PackageDefApi)(
    containingPackages: List[Name] = template.containingPackages,
    name: Name = template.name,
      members: List[Tree] = template.members): PackageDefApi =
    TC.copyPackageDef(template)(containingPackages, name, members)

    /** @see [[arrooj.ast.TreeCopiers.copyClassDef]] */
  def copyClassDef(template: ClassDefApi)(mods: Flags = template.mods,
      name: Name = template.name,
      parents: List[UseTree] = template.parents,
      body: TemplateApi = template.body): ClassDefApi =
    TC.copyClassDef(template)(mods, name, parents, body)


    /** @see [[arrooj.ast.TreeCopiers.copyTemplate]] */
  def copyTemplate(template: TemplateApi)(
      members: List[Tree] = template.members): TemplateApi =
    TC.copyTemplate(template)(members)


  def copyNew(template: NewApi)(app: ApplyApi = template.app): NewApi =
    TC.copyNew(template)(app)


  /** @see [[arrooj.ast.TreeCopiers.copySelect]] */
  def copySelect(template: SelectApi)(qual: Tree = template.qual,
    tree: SimpleUseTree = template.tree): SelectApi =
    TC.copySelect(template)(qual, tree)

    /** @see [[arrooj.ast.TreeCopiers.copyThis]] */
  def copyThis(template: ThisApi)(): ThisApi =
    TC.copyThis(template)()

  /** @see [[arrooj.ast.TreeCopiers.copySuper]] */
  def copySuper(template: SuperApi)(): SuperApi =
    TC.copySuper(template)()


  // tiny
  /** @see [[arrooj.ast.TreeCopiers.copyIdent]] */
  def copyIdent(template: IdentApi)
            (name: Name): IdentApi = {
    TC.copyIdent(template)(name)
  }

  /** @see [[arrooj.ast.TreeCopiers.copyTypeUse]] */
  def copyTypeUse(template: TypeUseApi)(name: Name): TypeUseApi = {
    TC.copyTypeUse(template)(name)
  }
  // calcj

  /** @see [[arrooj.ast.TreeCopiers.copyCast]] */
  def copyCast(template: CastApi)(
      tpt: UseTree = template.tpt,
      expr: Expr = template.expr): CastApi = {
    TC.copyCast(template)(tpt, expr)
  }


  /** @see [[arrooj.ast.TreeCopiers.copyLiteral]] */
  def copyLiteral(template: LiteralApi)
      (constant: Constant): LiteralApi = {
    TC.copyLiteral(template)(constant)
  }


  /** @see [[arrooj.ast.TreeCopiers.copyBinary]] */
  def copyBinary(template: BinaryApi)(lhs: Expr = template.lhs,
      op: BOp = template.op, rhs: Expr = template.rhs): BinaryApi = {
    TC.copyBinary(template)(lhs, op, rhs)
  }

  /** @see [[arrooj.ast.TreeCopiers.copyUnary]] */
  def copyUnary(template: UnaryApi)(isPostfix: Boolean = template.isPostfix,
    op: UOp = template.op, expr: Expr = template.expr): UnaryApi = {
    TC.copyUnary(template)(isPostfix, op, expr)
  }
  // primj

  /** @see [[arrooj.ast.TreeCopiers.copyAssign]] */
  def copyAssign(template: AssignApi)(lhs: Expr = template.lhs,
    rhs: Expr = template.rhs): AssignApi = {
    TC.copyAssign(template)(lhs, rhs)
  }


  /** @see [[arrooj.ast.TreeCopiers.copyIf]] */
  def copyIf(template: IfApi)(cond: Expr = template.cond,
    thenp: Expr = template.thenp, elsep: Expr = template.elsep): IfApi = {
    TC.copyIf(template)(cond, thenp, elsep)
  }


  /** @see [[arrooj.ast.TreeCopiers.copyWhile]] */
  def copyWhile(template: WhileApi)(isDoWhile: Boolean = template.isDoWhile,
    cond: Expr = template.cond, body: Expr = template.body): WhileApi = {
    TC.copyWhile(template)(isDoWhile, cond, body)
  }

  /** @see [[arrooj.ast.TreeCopiers.copyFor]] */
  def copyFor(template: ForApi)(inits: List[Tree] = template.inits,
    cond: Expr = template.cond, steps: List[Expr] = template.steps,
    body: Expr = template.body): ForApi = {
    TC.copyFor(template)(inits, cond, steps, body)
  }

  /** @see [[arrooj.ast.TreeCopiers.copyBlock]] */
  def copyBlock(template: BlockApi)(stmts: List[Tree] =
    template.stmts): BlockApi = {
    TC.copyBlock(template)(stmts)
  }

  /** @see [[arrooj.ast.TreeCopiers.copyTernary]] */
  def copyTernary(template: TernaryApi)(cond: Expr = template.cond,
    thenp: Expr = template.thenp,
    elsep: Expr = template.elsep): TernaryApi = {
    TC.copyTernary(template)(cond, thenp, elsep)
  }


  /** @see [[arrooj.ast.TreeCopiers.copyApply]] */
  def copyApply(template: ApplyApi)(fun: Expr = template.fun,
    args: List[Expr] = template.args): ApplyApi = {
    TC.copyApply(template)(fun, args)
  }

  /** @see [[arrooj.ast.TreeCopiers.copyReturn]] */
  def copyReturn(template: ReturnApi)(expr: Option[Expr] =
      template.expr): ReturnApi = {
    TC.copyReturn(template)(expr)
  }


  /** @see [[arrooj.ast.TreeCopiers.copyValDef]] */
  def copyValDef(template: ValDefApi)(mods: Flags = template.mods,
    tpt: UseTree = template.tpt, name: Name = template.name,
    rhs: Expr = template.rhs): ValDefApi = {
    TC.copyValDef(template)(mods, tpt, name, rhs)
  }

  // brokenj

  /** @see [[arrooj.ast.TreeCopiers.copyLabel]] */
  def copyLabel(template: LabelApi)(name: Name = template.name,
    stmt: Expr = template.stmt): LabelApi = {
    TC.copyLabel(template)(name, stmt)
  }

  /** @see [[arrooj.ast.TreeCopiers.copyBreak]] */
  def copyBreak(template: BreakApi)(label:
    Option[Name] = template.label): BreakApi = {
    TC.copyBreak(template)(label)
  }

  /** @see [[arrooj.ast.TreeCopiers.copyContinue]] */
  def copyContinue(template: ContinueApi)(label: Option[Name] =
        template.label): ContinueApi = {
    TC.copyContinue(template)(label)
  }

  /** @see [[arrooj.ast.TreeCopiers.copyCase]] */
  def copyCase(template: CaseApi)(guards: List[Expr] = template.guards,
    body: Tree = template.body): CaseApi = {
    TC.copyCase(template)(guards, body)
  }

  /** @see [[arrooj.ast.TreeCopiers.copySwitch]] */
  def copySwitch(template: SwitchApi)(expr: Expr = template.expr,
    cases: List[CaseApi] = template.cases): SwitchApi = {
      TC.copySwitch(template)(expr, cases)
  }
}

object TreeCopiers extends TreeCopiers
