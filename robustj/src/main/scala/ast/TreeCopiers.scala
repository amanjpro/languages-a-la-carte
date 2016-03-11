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

  protected def copyProperties(template: Tree,
      newTree: Tree): Unit = newTree.attributes = template.attributes

  def copyThrow(template: ThrowApi)(expr: Expr = template.expr): ThrowApi = {
    val res = TreeFactories.mkThrow(expr)
    copyProperties(template, res)
    res
  }

  def copyTry(template: TryApi)(tryClause: BlockApi = template.tryClause,
      catches: List[CatchApi] = template.catches,
      finallyClause: Option[BlockApi] = template.finallyClause): TryApi = {
    val res = TreeFactories.mkTry(tryClause, catches, finallyClause)
    copyProperties(template, res)
    res
  }

  def copyCatch(template: CatchApi)(eparam: ValDefApi = template.eparam,
    catchClause: BlockApi = template.catchClause): CatchApi = {
    val res = TreeFactories.mkCatch(eparam, catchClause)
    copyProperties(template, res)
    res
  }

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


  // ooj

  def copyProgram(template: ProgramApi)(members: List[Tree] =
    template.members): ProgramApi = {
    val res = TreeFactories.mkProgram(members)
    copyProperties(template, res)
    res
  }

  def copyCompilationUnit(template: CompilationUnitApi)(
    module: PackageDefApi = template.module,
    sourceName: String = template.sourceName,
    sourcePath: List[String] = template.sourcePath): CompilationUnitApi =
    TC.copyCompilationUnit(template)(module, sourceName, sourcePath)

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


