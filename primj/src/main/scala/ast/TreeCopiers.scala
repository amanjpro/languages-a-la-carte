package ch.usi.inf.l3.sana.primj.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.tiny.ast.Implicits._
import sana.tiny.modifiers.Flags
import sana.tiny.ast._
import sana.calcj.ast._
import sana.primj.ast._



trait TreeCopiers extends sana.calcj.ast.TreeCopiers {
  def copyProgram(template: ProgramApi)(members: List[DefTree] =
    template.members,
    sourceName: String = template.sourceName): ProgramApi = {
    val res = Program(members, sourceName)
    copyProperties(template, res)
    res
  }


  def copyAssign(template: AssignApi)(lhs: Expr = template.lhs,
    rhs: Expr = template.rhs): AssignApi = {
    val res = Assign(lhs, rhs)
    copyProperties(template, res)
    res
  }


  def copyIf(template: IfApi)(cond: Expr = template.cond,
    thenp: Expr = template.thenp, elsep: Expr = template.elsep): IfApi = {
    val res = If(cond, thenp, elsep)
    copyProperties(template, res)
    res
  }


  def copyWhile(template: WhileApi)(isDoWhile: Boolean = template.isDoWhile,
    cond: Expr = template.cond, body: Expr = template.body): WhileApi = {
    val res = While(isDoWhile, cond, body)
    copyProperties(template, res)
    res
  }

  def copyFor(template: ForApi)(inits: List[Tree] = template.inits,
    cond: Expr = template.cond, steps: List[Expr] = template.steps,
    body: Expr = template.body): ForApi = {
    val res = For(inits, cond, steps, body)
    copyProperties(template, res)
    res
  }

  def copyBlock(template: BlockApi)(stmts: List[Tree] =
    template.stmts): BlockApi = {
    val res = Block(stmts)
    copyProperties(template, res)
    res
  }

  def copyTernary(template: TernaryApi)(cond: Expr = template.cond,
    thenp: Expr = template.thenp,
    elsep: Expr = template.elsep): TernaryApi = {
    val res = Ternary(cond, thenp, elsep)
    copyProperties(template, res)
    res
  }


  def copyApply(template: ApplyApi)(fun: Expr = template.fun,
    args: List[Expr] = template.args): ApplyApi = {

    val res = Apply(fun, args)
    copyProperties(template, res)
    res
  }

  def copyReturn(template: ReturnApi)(expr: Option[Expr] =
      template.expr): ReturnApi = {
    val res = Return(expr)
    copyProperties(template, res)
    res
  }


  def copyMethodDef(template: MethodDefApi)(ret: UseTree = template.ret,
    name: Name = template.name, params: List[ValDefApi]  = template.params,
    body: Expr = template.body): MethodDefApi = {
    val res = MethodDef(ret, name, params, body)
    copyProperties(template, res)
    res
  }

  def copyValDef(template: ValDefApi)(mods: Flags = template.mods,
    tpt: UseTree = template.tpt, name: Name = template.name,
    rhs: Expr = template.rhs): ValDefApi = {
    val res = ValDef(mods, tpt, name, rhs)
    copyProperties(template, res)
    res
  }
}

object TreeCopiers extends TreeCopiers


