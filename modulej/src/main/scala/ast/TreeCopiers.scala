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
import sana.tiny.symbols.Symbol
import sana.tiny.ast.Implicits._
import sana.tiny.modifiers.Flags
import sana.tiny.names.Name
import sana.tiny.ast._
import sana.calcj.ast._
import sana.brokenj.ast._
import sana.arrayj.ast._
import sana.primj.ast.{MethodDefApi => _, ProgramApi => _, _}
import sana.ooj.ast.{MethodDefApi => _, CompilationUnitApi => _, _}
import sana.robustj.ast._
import sana.ppj.ast._
import sana.ppj.ast.{TreeCopiers => TC}
import sana.calcj.ast.operators.{BOp, UOp}





trait TreeCopiers {

  /**
   * Copies the properties (attributes) of a tree to another
   *
   * @param template the tree to copy its attributes
   * @param newTree the tree to copy the attributes to
   */
  protected def copyProperties(template: Tree,
      newTree: Tree): Unit = newTree.attributes = template.attributes

  /**
   * Returns a copy of an "import" statement
   *
   * @param template the tree to be copied
   * @param qual the fully qualified imported name
   * @param isOnDemand is this import an on-demand statement
   * @param pos the position of this tree
   * @param owner the owner of this tree
   */
  def copyImport(template: ImportApi)(
      qual: UseTree = template.qual,
      isOnDemand: Boolean = template.isOnDemand,
      pos: Option[Position] = template.pos,
      owner: Option[Symbol]  = template.owner): ImportApi = {
    val res = TreeFactories.mkImport(qual, isOnDemand)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a "compilation-unit" tree
   *
   * @param template the tree to be copied
   * @param imports the list of imports defined in this compilation unit
   * @param module the package in this compilation unit
   * @param sourceName the name of the source file of this compilation unit
   * @param sorucePath the path of the source file of this compilation unit
   */
  def copyCompilationUnit(template: CompilationUnitApi)(
    imports: List[ImportApi] = template.imports,
    module: PackageDefApi = template.module,
    sourceName: String = template.sourceName,
    sourcePath: List[String] = template.sourcePath): CompilationUnitApi = {
    val res = TreeFactories.mkCompilationUnit(imports, module, sourceName,
      sourcePath)
    copyProperties(template, res)
    res
  }

  // ppj
  /** @see {{{sana.ppj.ast.TreeCopiers.copySynchronized}}} */
  def copySynchronized(template: SynchronizedApi)(
      expr: Expr = template.expr, block: BlockApi = template.block,
      pos: Option[Position] = template.pos,
      owner: Option[Symbol]  = template.owner): SynchronizedApi =
    sana.ppj.ast.TreeCopiers.copySynchronized(template)(expr, block, pos, owner)

  // arrooj
  /** @see [[sana.ppj.ast.TreeCopiers.copyArrayInitializer]] */
  def copyArrayInitializer(template: ArrayInitializerApi)(
      elements: List[Expr] = template.elements): ArrayInitializerApi =
    sana.ppj.ast.TreeCopiers.copyArrayInitializer(template)(elements)


  /** @see [[sana.ppj.ast.TreeCopiers.copyArrayAccess]] */
  def copyArrayAccess(template: ArrayAccessApi)(
    array: Expr = template.array,
    index: Expr = template.index): ArrayAccessApi =
    sana.ppj.ast.TreeCopiers.copyArrayAccess(template)(array, index)

  /** @see [[sana.ppj.ast.TreeCopiers.copyArrayTypeUse]] */
  def copyArrayTypeUse(template: ArrayTypeUseApi)(
      tpt: UseTree = template.tpt): ArrayTypeUseApi =
    sana.ppj.ast.TreeCopiers.copyArrayTypeUse(template)(tpt)

  /** @see [[sana.ppj.ast.TreeCopiers.copyArrayCreation]] */
  def copyArrayCreation(template: ArrayCreationApi)(
      array: Expr = template.array,
      size: Option[Expr] = template.size): ArrayCreationApi =
    sana.ppj.ast.TreeCopiers.copyArrayCreation(template)(array, size)


  // robustj
  /** @see [[sana.ppj.ast.TreeCopiers.copyThrow]] */
  def copyThrow(template: ThrowApi)(expr: Expr = template.expr): ThrowApi = {
    sana.ppj.ast.TreeCopiers.copyThrow(template)(expr)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyTry]] */
  def copyTry(template: TryApi)(tryClause: BlockApi = template.tryClause,
      catches: List[CatchApi] = template.catches,
      finallyClause: Option[BlockApi] = template.finallyClause): TryApi = {
    sana.ppj.ast.TreeCopiers.copyTry(template)(tryClause, catches, finallyClause)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyCatch]] */
  def copyCatch(template: CatchApi)(eparam: ValDefApi = template.eparam,
    catchClause: BlockApi = template.catchClause): CatchApi = {
    sana.ppj.ast.TreeCopiers.copyCatch(template)(eparam, catchClause)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyMethodDef]] */
  def copyMethodDef(template: MethodDefApi)(mods: Flags = template.mods,
    ret: UseTree = template.ret,
    name: Name = template.name, params: List[ValDefApi]  = template.params,
    throwsClause: List[UseTree] = template.throwsClause,
    body: Expr = template.body): MethodDefApi = {
    sana.ppj.ast.TreeCopiers.copyMethodDef(template)(mods, ret, name, params, throwsClause, body)
  }


  // ooj

  /** @see [[sana.ppj.ast.TreeCopiers.copyProgram]] */
  def copyProgram(template: ProgramApi)(members: List[Tree] =
    template.members): ProgramApi = {
    sana.ppj.ast.TreeCopiers.copyProgram(template)(members)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyPackageDef]] */
  def copyPackageDef(template: PackageDefApi)(
    containingPackages: List[Name] = template.containingPackages,
    name: Name = template.name,
      members: List[Tree] = template.members): PackageDefApi =
    sana.ppj.ast.TreeCopiers.copyPackageDef(template)(containingPackages, name, members)

  /** @see [[sana.ppj.ast.TreeCopiers.copyClassDef]] */
  def copyClassDef(template: ClassDefApi)(mods: Flags = template.mods,
      name: Name = template.name,
      parents: List[UseTree] = template.parents,
      body: TemplateApi = template.body): ClassDefApi =
    sana.ppj.ast.TreeCopiers.copyClassDef(template)(mods, name, parents, body)


  /** @see [[sana.ppj.ast.TreeCopiers.copyTemplate]] */
  def copyTemplate(template: TemplateApi)(
      members: List[Tree] = template.members): TemplateApi =
    sana.ppj.ast.TreeCopiers.copyTemplate(template)(members)


  /** @see [[sana.ppj.ast.TreeCopiers.copyNew]] */
  def copyNew(template: NewApi)(app: ApplyApi = template.app): NewApi =
    sana.ppj.ast.TreeCopiers.copyNew(template)(app)


  /** @see [[sana.ppj.ast.TreeCopiers.copySelect]] */
  def copySelect(template: SelectApi)(qual: Tree = template.qual,
    tree: SimpleUseTree = template.tree): SelectApi =
    sana.ppj.ast.TreeCopiers.copySelect(template)(qual, tree)

  /** @see [[sana.ppj.ast.TreeCopiers.copyThis]] */
  def copyThis(template: ThisApi)(): ThisApi =
    sana.ppj.ast.TreeCopiers.copyThis(template)()

  /** @see [[sana.ppj.ast.TreeCopiers.copySuper]] */
  def copySuper(template: SuperApi)(): SuperApi =
    sana.ppj.ast.TreeCopiers.copySuper(template)()


  // tiny
  /** @see [[sana.ppj.ast.TreeCopiers.copyIdent]] */
  def copyIdent(template: IdentApi)
            (name: Name): IdentApi = {
    sana.ppj.ast.TreeCopiers.copyIdent(template)(name)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyTypeUse]] */
  def copyTypeUse(template: TypeUseApi)(name: Name): TypeUseApi = {
    sana.ppj.ast.TreeCopiers.copyTypeUse(template)(name)
  }
  // calcj

  /** @see [[sana.ppj.ast.TreeCopiers.copyCast]] */
  def copyCast(template: CastApi)(
      tpt: UseTree = template.tpt,
      expr: Expr = template.expr): CastApi = {
    sana.ppj.ast.TreeCopiers.copyCast(template)(tpt, expr)
  }


  /** @see [[sana.ppj.ast.TreeCopiers.copyLiteral]] */
  def copyLiteral(template: LiteralApi)
      (constant: Constant): LiteralApi = {
    sana.ppj.ast.TreeCopiers.copyLiteral(template)(constant)
  }


  /** @see [[sana.ppj.ast.TreeCopiers.copyBinary]] */
  def copyBinary(template: BinaryApi)(lhs: Expr = template.lhs,
      op: BOp = template.op, rhs: Expr = template.rhs): BinaryApi = {
    sana.ppj.ast.TreeCopiers.copyBinary(template)(lhs, op, rhs)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyUnary]] */
  def copyUnary(template: UnaryApi)(isPostfix: Boolean = template.isPostfix,
    op: UOp = template.op, expr: Expr = template.expr): UnaryApi = {
    sana.ppj.ast.TreeCopiers.copyUnary(template)(isPostfix, op, expr)
  }
  // primj

  /** @see [[sana.ppj.ast.TreeCopiers.copyAssign]] */
  def copyAssign(template: AssignApi)(lhs: Expr = template.lhs,
    rhs: Expr = template.rhs): AssignApi = {
    sana.ppj.ast.TreeCopiers.copyAssign(template)(lhs, rhs)
  }


  /** @see [[sana.ppj.ast.TreeCopiers.copyIf]] */
  def copyIf(template: IfApi)(cond: Expr = template.cond,
    thenp: Expr = template.thenp, elsep: Expr = template.elsep): IfApi = {
    sana.ppj.ast.TreeCopiers.copyIf(template)(cond, thenp, elsep)
  }


  /** @see [[sana.ppj.ast.TreeCopiers.copyWhile]] */
  def copyWhile(template: WhileApi)(isDoWhile: Boolean = template.isDoWhile,
    cond: Expr = template.cond, body: Expr = template.body): WhileApi = {
    sana.ppj.ast.TreeCopiers.copyWhile(template)(isDoWhile, cond, body)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyFor]] */
  def copyFor(template: ForApi)(inits: List[Tree] = template.inits,
    cond: Expr = template.cond, steps: List[Expr] = template.steps,
    body: Expr = template.body): ForApi = {
    sana.ppj.ast.TreeCopiers.copyFor(template)(inits, cond, steps, body)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyBlock]] */
  def copyBlock(template: BlockApi)(stmts: List[Tree] =
    template.stmts): BlockApi = {
    sana.ppj.ast.TreeCopiers.copyBlock(template)(stmts)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyTernary]] */
  def copyTernary(template: TernaryApi)(cond: Expr = template.cond,
    thenp: Expr = template.thenp,
    elsep: Expr = template.elsep): TernaryApi = {
    sana.ppj.ast.TreeCopiers.copyTernary(template)(cond, thenp, elsep)
  }


  /** @see [[sana.ppj.ast.TreeCopiers.copyApply]] */
  def copyApply(template: ApplyApi)(fun: Expr = template.fun,
    args: List[Expr] = template.args): ApplyApi = {
    sana.ppj.ast.TreeCopiers.copyApply(template)(fun, args)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyReturn]] */
  def copyReturn(template: ReturnApi)(expr: Option[Expr] =
      template.expr): ReturnApi = {
    sana.ppj.ast.TreeCopiers.copyReturn(template)(expr)
  }


  /** @see [[sana.ppj.ast.TreeCopiers.copyValDef]] */
  def copyValDef(template: ValDefApi)(mods: Flags = template.mods,
    tpt: UseTree = template.tpt, name: Name = template.name,
    rhs: Expr = template.rhs): ValDefApi = {
    sana.ppj.ast.TreeCopiers.copyValDef(template)(mods, tpt, name, rhs)
  }

  // brokenj

  /** @see [[sana.ppj.ast.TreeCopiers.copyLabel]] */
  def copyLabel(template: LabelApi)(name: Name = template.name,
    stmt: Expr = template.stmt): LabelApi = {
    sana.ppj.ast.TreeCopiers.copyLabel(template)(name, stmt)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyBreak]] */
  def copyBreak(template: BreakApi)(label:
    Option[Name] = template.label): BreakApi = {
    sana.ppj.ast.TreeCopiers.copyBreak(template)(label)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyContinue]] */
  def copyContinue(template: ContinueApi)(label: Option[Name] =
        template.label): ContinueApi = {
    sana.ppj.ast.TreeCopiers.copyContinue(template)(label)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copyCase]] */
  def copyCase(template: CaseApi)(guards: List[Expr] = template.guards,
    body: Tree = template.body): CaseApi = {
    sana.ppj.ast.TreeCopiers.copyCase(template)(guards, body)
  }

  /** @see [[sana.ppj.ast.TreeCopiers.copySwitch]] */
  def copySwitch(template: SwitchApi)(expr: Expr = template.expr,
    cases: List[CaseApi] = template.cases): SwitchApi = {
      sana.ppj.ast.TreeCopiers.copySwitch(template)(expr, cases)
  }
}

object TreeCopiers extends TreeCopiers
