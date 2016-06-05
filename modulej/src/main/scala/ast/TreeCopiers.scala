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

  protected def copyProperties(template: Tree,
      newTree: Tree): Unit = newTree.attributes = template.attributes

  def copyImport(template: ImportApi)(
      qual: UseTree = template.qual,
      isOnDemand: Boolean = template.isOnDemand,
      pos: Option[Position] = template.pos,
      owner: Option[Symbol]  = template.owner): ImportApi = {
    val res = TreeFactories.mkImport(qual, isOnDemand)
    copyProperties(template, res)
    res
  }

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
  def copySynchronized(template: SynchronizedApi)(
      expr: Expr = template.expr, block: BlockApi = template.block,
      pos: Option[Position] = template.pos,
      owner: Option[Symbol]  = template.owner): SynchronizedApi =
    TC.copySynchronized(template)(expr, block, pos, owner)

  // arrooj
  def copyArrayInitializer(template: ArrayInitializerApi)(
      elements: List[Expr] = template.elements): ArrayInitializerApi =
    TC.copyArrayInitializer(template)(elements)


  def copyArrayAccess(template: ArrayAccessApi)(
    array: Expr = template.array,
    index: Expr = template.index): ArrayAccessApi =
    TC.copyArrayAccess(template)(array, index)

  def copyArrayTypeUse(template: ArrayTypeUseApi)(
      tpt: UseTree = template.tpt): ArrayTypeUseApi =
    TC.copyArrayTypeUse(template)(tpt)

  def copyArrayCreation(template: ArrayCreationApi)(
      array: Expr = template.array,
      size: Option[Expr] = template.size): ArrayCreationApi =
    TC.copyArrayCreation(template)(array, size)


  // robustj
  def copyThrow(template: ThrowApi)(expr: Expr = template.expr): ThrowApi = {
    TC.copyThrow(template)(expr)
  }

  def copyTry(template: TryApi)(tryClause: BlockApi = template.tryClause,
      catches: List[CatchApi] = template.catches,
      finallyClause: Option[BlockApi] = template.finallyClause): TryApi = {
    TC.copyTry(template)(tryClause, catches, finallyClause)
  }

  def copyCatch(template: CatchApi)(eparam: ValDefApi = template.eparam,
    catchClause: BlockApi = template.catchClause): CatchApi = {
    TC.copyCatch(template)(eparam, catchClause)
  }

  def copyMethodDef(template: MethodDefApi)(mods: Flags = template.mods,
    ret: UseTree = template.ret,
    name: Name = template.name, params: List[ValDefApi]  = template.params,
    throwsClause: List[UseTree] = template.throwsClause,
    body: Expr = template.body): MethodDefApi = {
    TC.copyMethodDef(template)(mods, ret, name, params, throwsClause, body)
  }


  // ooj

  def copyProgram(template: ProgramApi)(members: List[Tree] =
    template.members): ProgramApi = {
    TC.copyProgram(template)(members)
  }

  def copyPackageDef(template: PackageDefApi)(
    containingPackages: List[Name] = template.containingPackages,
    name: Name = template.name,
      members: List[Tree] = template.members): PackageDefApi =
    TC.copyPackageDef(template)(containingPackages, name, members)

  def copyClassDef(template: ClassDefApi)(mods: Flags = template.mods,
      name: Name = template.name,
      parents: List[UseTree] = template.parents,
      body: TemplateApi = template.body): ClassDefApi =
    TC.copyClassDef(template)(mods, name, parents, body)


  def copyTemplate(template: TemplateApi)(
      members: List[Tree] = template.members): TemplateApi =
    TC.copyTemplate(template)(members)


  def copyNew(template: NewApi)(app: ApplyApi = template.app): NewApi =
    TC.copyNew(template)(app)


  def copySelect(template: SelectApi)(qual: Tree = template.qual,
    tree: SimpleUseTree = template.tree): SelectApi =
    TC.copySelect(template)(qual, tree)

  def copyThis(template: ThisApi)(): ThisApi =
    TC.copyThis(template)()

  def copySuper(template: SuperApi)(): SuperApi =
    TC.copySuper(template)()


  // tiny
  def copyIdent(template: IdentApi)
            (name: Name): IdentApi = {
    TC.copyIdent(template)(name)
  }

  def copyTypeUse(template: TypeUseApi)(name: Name): TypeUseApi = {
    TC.copyTypeUse(template)(name)
  }
  // calcj

  def copyCast(template: CastApi)(
      tpt: UseTree = template.tpt,
      expr: Expr = template.expr): CastApi = {
    TC.copyCast(template)(tpt, expr)
  }


  def copyLiteral(template: LiteralApi)
      (constant: Constant): LiteralApi = {
    TC.copyLiteral(template)(constant)
  }


  def copyBinary(template: BinaryApi)(lhs: Expr = template.lhs,
      op: BOp = template.op, rhs: Expr = template.rhs): BinaryApi = {
    TC.copyBinary(template)(lhs, op, rhs)
  }

  def copyUnary(template: UnaryApi)(isPostfix: Boolean = template.isPostfix,
    op: UOp = template.op, expr: Expr = template.expr): UnaryApi = {
    TC.copyUnary(template)(isPostfix, op, expr)
  }
  // primj

  def copyAssign(template: AssignApi)(lhs: Expr = template.lhs,
    rhs: Expr = template.rhs): AssignApi = {
    TC.copyAssign(template)(lhs, rhs)
  }


  def copyIf(template: IfApi)(cond: Expr = template.cond,
    thenp: Expr = template.thenp, elsep: Expr = template.elsep): IfApi = {
    TC.copyIf(template)(cond, thenp, elsep)
  }


  def copyWhile(template: WhileApi)(isDoWhile: Boolean = template.isDoWhile,
    cond: Expr = template.cond, body: Expr = template.body): WhileApi = {
    TC.copyWhile(template)(isDoWhile, cond, body)
  }

  def copyFor(template: ForApi)(inits: List[Tree] = template.inits,
    cond: Expr = template.cond, steps: List[Expr] = template.steps,
    body: Expr = template.body): ForApi = {
    TC.copyFor(template)(inits, cond, steps, body)
  }

  def copyBlock(template: BlockApi)(stmts: List[Tree] =
    template.stmts): BlockApi = {
    TC.copyBlock(template)(stmts)
  }

  def copyTernary(template: TernaryApi)(cond: Expr = template.cond,
    thenp: Expr = template.thenp,
    elsep: Expr = template.elsep): TernaryApi = {
    TC.copyTernary(template)(cond, thenp, elsep)
  }


  def copyApply(template: ApplyApi)(fun: Expr = template.fun,
    args: List[Expr] = template.args): ApplyApi = {
    TC.copyApply(template)(fun, args)
  }

  def copyReturn(template: ReturnApi)(expr: Option[Expr] =
      template.expr): ReturnApi = {
    TC.copyReturn(template)(expr)
  }


  def copyValDef(template: ValDefApi)(mods: Flags = template.mods,
    tpt: UseTree = template.tpt, name: Name = template.name,
    rhs: Expr = template.rhs): ValDefApi = {
    TC.copyValDef(template)(mods, tpt, name, rhs)
  }

  // brokenj

  def copyLabel(template: LabelApi)(name: Name = template.name,
    stmt: Expr = template.stmt): LabelApi = {
    TC.copyLabel(template)(name, stmt)
  }

  def copyBreak(template: BreakApi)(label:
    Option[Name] = template.label): BreakApi = {
    TC.copyBreak(template)(label)
  }

  def copyContinue(template: ContinueApi)(label: Option[Name] =
        template.label): ContinueApi = {
    TC.copyContinue(template)(label)
  }

  def copyCase(template: CaseApi)(guards: List[Expr] = template.guards,
    body: Tree = template.body): CaseApi = {
    TC.copyCase(template)(guards, body)
  }

  def copySwitch(template: SwitchApi)(expr: Expr = template.expr,
    cases: List[CaseApi] = template.cases): SwitchApi = {
      TC.copySwitch(template)(expr, cases)
  }
}

object TreeCopiers extends TreeCopiers
