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
import sana.primj.ast._
import sana.brokenj.ast.{TreeCopiers => TC, _}



trait TreeCopiers extends {

  protected def copyProperties(template: Tree,
      newTree: Tree): Unit = newTree.attributes = template.attributes

  def copyCompilationUnit(template: CompilationUnitApi)(
    module: PackageDefApi = template.module,
    sourceName: String = template.sourceName,
    sourcePath: List[String] = template.sourcePath): CompilationUnitApi = {
    val res = CompilationUnit(module, sourceName, sourcePath)
    copyProperties(template, res)
    res
  }

  def copyPackageDef(template: PackageDefApi)(mods: Flags = template.mods,
      name: Name = template.name,
      members: List[Tree] = template.members): PackageDefApi = {
    val res = PackageDef(mods, name, members)
    copyProperties(template, res)
    res
  }

  def copyClassDef(template: ClassDefApi)(mods: Flags = template.mods,
      name: Name = template.name,
      parents: List[UseTree] = template.parents,
      body: TemplateApi = template.body): ClassDefApi = {
    val res = ClassDef(mods, name, parents, body)
    copyProperties(template, res)
    res
  }


  def copyTemplate(template: TemplateApi)(
      members: List[Tree] = template.members): TemplateApi = {
    val res = Template(members)
    copyProperties(template, res)
    res
  }


  def copyNew(template: NewApi)(tpt: UseTree = template.tpt,
    args: List[Expr] = template.args): NewApi = {
    val res = New(tpt, args)
    copyProperties(template, res)
    res
  }


  def copySelect(template: SelectApi)(qual: Tree = template.qual,
    tree: SimpleUseTree = template.tree): SelectApi = {
    val res = Select(qual, tree)
    copyProperties(template, res)
    res
  }

  def copyThis(template: ThisApi)(): ThisApi = {
    val res = new This
    copyProperties(template, res)
    res
  }

  def copySuper(template: SuperApi)(): SuperApi = {
    val res = new Super
    copyProperties(template, res)
    res
  }


  def copyMethodDef(template: MethodDefApi)(mods: Flags = template.mods,
    ret: UseTree = template.ret,
    name: Name = template.name, params: List[ValDefApi]  = template.params,
    body: Expr = template.body): MethodDefApi = {
    val res = MethodDef(mods, ret, name, params, body)
    copyProperties(template, res)
    res
  }


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


