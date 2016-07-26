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

package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.tiny.ast.Implicits._
import sana.tiny.modifiers.Flags
import sana.tiny.ast._
import sana.calcj.ast._
import sana.calcj.ast.operators.{BOp, UOp}
import sana.primj.ast.{ProgramApi => _, _}
import sana.brokenj.ast.{TreeCopiers => TC, _}



trait TreeCopiers {

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyProperties}}} */
  protected def copyProperties(template: Tree,
      newTree: Tree): Unit = newTree.attributes = template.attributes
  /**
   * Returns a copy of a "program" tree
   *
   * @param template the tree to be copied
   * @param members the members of this program
   */
  def copyProgram(template: ProgramApi)(members: List[Tree] =
    template.members): ProgramApi = {
    val res = TreeFactories.mkProgram(members)
    copyProperties(template, res)
    res
  }


  /**
   * Returns a copy of a "compilation-unit" tree
   *
   * @param template the tree to be copied
   * @param module the package in this compilation unit
   * @param sourceName the name of the source file of this compilation unit
   * @param sorucePath the path of the source file of this compilation unit
   */
  def copyCompilationUnit(template: CompilationUnitApi)(
    module: PackageDefApi = template.module,
    sourceName: String = template.sourceName,
    sourcePath: List[String] = template.sourcePath): CompilationUnitApi = {
    val res = TreeFactories.mkCompilationUnit(module,
      sourceName, sourcePath)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a package
   *
   * @param template the tree to be copied
   * @param containingPackages the packages that contains this package
   * @param members the members of this package
   */
  def copyPackageDef(template: PackageDefApi)(
    containingPackages: List[Name] = template.containingPackages,
    name: Name = template.name,
      members: List[Tree] = template.members): PackageDefApi = {
    val res = TreeFactories.mkPackageDef(containingPackages, name, members)
    copyProperties(template, res)
    res
  }


  /**
   * Returns a copy of a class
   *
   * @param template the tree to be copied
   * @param mods the modifiers of this class
   * @param name the name of this class
   * @param parents the parents of this class
   * @param body the body of this class
   */
  def copyClassDef(template: ClassDefApi)(mods: Flags = template.mods,
      name: Name = template.name,
      parents: List[UseTree] = template.parents,
      body: TemplateApi = template.body): ClassDefApi = {
    val res = TreeFactories.mkClassDef(mods, name, parents, body)
    copyProperties(template, res)
    res
  }


  /**
   * Returns a copy of a body of a class
   *
   * @param template the tree to be copied
   * @param members the members of the body of the class
   */
  def copyTemplate(template: TemplateApi)(
      members: List[Tree] = template.members): TemplateApi = {
    val res = TreeFactories.mkTemplate(members)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a `new` expression
   *
   * @param template the tree to be copied
   * @param app the method/function application of this `new`
   */
  def copyNew(template: NewApi)(app: ApplyApi = template.app): NewApi = {
    val res = TreeFactories.mkNew(app)
    copyProperties(template, res)
    res
  }


  /**
   * Returns a copy of a `select` expression
   *
   * @param template the tree to be copied
   * @param qual the tree that has been selected from
   * @param tree the tree that has been selected
   */
  def copySelect(template: SelectApi)(qual: Tree = template.qual,
    tree: SimpleUseTree = template.tree): SelectApi = {
    val res = TreeFactories.mkSelect(qual, tree)
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a `this` expression
   *
   * @param template the tree to be copied
   */
  def copyThis(template: ThisApi)(): ThisApi = {
    val res = TreeFactories.mkThis()
    copyProperties(template, res)
    res
  }

  /**
   * Returns a copy of a `super` expression
   *
   * @param template the tree to be copied
   */
  def copySuper(template: SuperApi)(): SuperApi = {
    val res = TreeFactories.mkSuper()
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
   * @param body the body of this method/function
   */
  def copyMethodDef(template: MethodDefApi)(mods: Flags = template.mods,
    ret: UseTree = template.ret,
    name: Name = template.name, params: List[ValDefApi]  = template.params,
    body: Expr = template.body): MethodDefApi = {
    val res = TreeFactories.mkMethodDef(mods, ret, name, params, body)
    copyProperties(template, res)
    res
  }


  // tiny
  /** @see {{{sana.brokenj.ast.TreeCopiers.copyIdent}}} */
  def copyIdent(template: IdentApi)
            (name: Name): IdentApi = {
    TC.copyIdent(template)(name)
  }

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyTypeUse}}} */
  def copyTypeUse(template: TypeUseApi)(name: Name): TypeUseApi = {
    TC.copyTypeUse(template)(name)
  }
  // calcj

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyCast}}} */
  def copyCast(template: CastApi)(
      tpt: UseTree = template.tpt,
      expr: Expr = template.expr): CastApi = {
    TC.copyCast(template)(tpt, expr)
  }


  /** @see {{{sana.brokenj.ast.TreeCopiers.copyLiteral}}} */
  def copyLiteral(template: LiteralApi)
      (constant: Constant): LiteralApi = {
    TC.copyLiteral(template)(constant)
  }


  /** @see {{{sana.brokenj.ast.TreeCopiers.copyBinary}}} */
  def copyBinary(template: BinaryApi)(lhs: Expr = template.lhs,
      op: BOp = template.op, rhs: Expr = template.rhs): BinaryApi = {
    TC.copyBinary(template)(lhs, op, rhs)
  }

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyUnary}}} */
  def copyUnary(template: UnaryApi)(isPostfix: Boolean = template.isPostfix,
    op: UOp = template.op, expr: Expr = template.expr): UnaryApi = {
    TC.copyUnary(template)(isPostfix, op, expr)
  }
  // primj

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyAssign}}} */
  def copyAssign(template: AssignApi)(lhs: Expr = template.lhs,
    rhs: Expr = template.rhs): AssignApi = {
    TC.copyAssign(template)(lhs, rhs)
  }


  /** @see {{{sana.brokenj.ast.TreeCopiers.copyIf}}} */
  def copyIf(template: IfApi)(cond: Expr = template.cond,
    thenp: Expr = template.thenp, elsep: Expr = template.elsep): IfApi = {
    TC.copyIf(template)(cond, thenp, elsep)
  }


  /** @see {{{sana.brokenj.ast.TreeCopiers.copyWhile}}} */
  def copyWhile(template: WhileApi)(isDoWhile: Boolean = template.isDoWhile,
    cond: Expr = template.cond, body: Expr = template.body): WhileApi = {
    TC.copyWhile(template)(isDoWhile, cond, body)
  }

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyFor}}} */
  def copyFor(template: ForApi)(inits: List[Tree] = template.inits,
    cond: Expr = template.cond, steps: List[Expr] = template.steps,
    body: Expr = template.body): ForApi = {
    TC.copyFor(template)(inits, cond, steps, body)
  }

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyBlock}}} */
  def copyBlock(template: BlockApi)(stmts: List[Tree] =
    template.stmts): BlockApi = {
    TC.copyBlock(template)(stmts)
  }

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyTernary}}} */
  def copyTernary(template: TernaryApi)(cond: Expr = template.cond,
    thenp: Expr = template.thenp,
    elsep: Expr = template.elsep): TernaryApi = {
    TC.copyTernary(template)(cond, thenp, elsep)
  }


  /** @see {{{sana.brokenj.ast.TreeCopiers.copyApply}}} */
  def copyApply(template: ApplyApi)(fun: Expr = template.fun,
    args: List[Expr] = template.args): ApplyApi = {
    TC.copyApply(template)(fun, args)
  }

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyReturn}}} */
  def copyReturn(template: ReturnApi)(expr: Option[Expr] =
      template.expr): ReturnApi = {
    TC.copyReturn(template)(expr)
  }


  /** @see {{{sana.brokenj.ast.TreeCopiers.copyValDef}}} */
  def copyValDef(template: ValDefApi)(mods: Flags = template.mods,
    tpt: UseTree = template.tpt, name: Name = template.name,
    rhs: Expr = template.rhs): ValDefApi = {
    TC.copyValDef(template)(mods, tpt, name, rhs)
  }

  // brokenj

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyLabel}}} */
  def copyLabel(template: LabelApi)(name: Name = template.name,
    stmt: Expr = template.stmt): LabelApi = {
    TC.copyLabel(template)(name, stmt)
  }

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyBreak}}} */
  def copyBreak(template: BreakApi)(label:
    Option[Name] = template.label): BreakApi = {
    TC.copyBreak(template)(label)
  }

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyContinue}}} */
  def copyContinue(template: ContinueApi)(label: Option[Name] =
        template.label): ContinueApi = {
    TC.copyContinue(template)(label)
  }

  /** @see {{{sana.brokenj.ast.TreeCopiers.copyCase}}} */
  def copyCase(template: CaseApi)(guards: List[Expr] = template.guards,
    body: Tree = template.body): CaseApi = {
    TC.copyCase(template)(guards, body)
  }

  /** @see {{{sana.brokenj.ast.TreeCopiers.copySwitch}}} */
  def copySwitch(template: SwitchApi)(expr: Expr = template.expr,
    cases: List[CaseApi] = template.cases): SwitchApi = {
      TC.copySwitch(template)(expr, cases)
  }
}

object TreeCopiers extends TreeCopiers
