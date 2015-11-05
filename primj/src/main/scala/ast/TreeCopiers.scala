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
  def copyProgram(template: Program)(members: List[DefTree] =
    template.members,
    sourceName: String = template.sourceName): Program = {
    val res = Program(members, sourceName)
    copyProperties(template, res)
    res
  }


  def copyAssign(template: Assign)(lhs: Expr = template.lhs,
    rhs: Expr = template.rhs): Assign = {
    val res = Assign(lhs, rhs)
    copyProperties(template, res)
    res
  }


  def copyIf(template: If)(cond: Expr = template.cond,
    thenp: Expr = template.thenp, elsep: Expr = template.elsep): If = {
    val res = If(cond, thenp, elsep)
    copyProperties(template, res)
    res
  }


  def copyWhile(template: While)(isDoWhile: Boolean = template.isDoWhile,
    cond: Expr = template.cond, body: Expr = template.body): While = {
    val res = While(isDoWhile, cond, body)
    copyProperties(template, res)
    res
  }

  def copyFor(template: For)(inits: List[Tree] = template.inits,
    cond: Expr = template.cond, steps: List[Expr] = template.steps,
    body: Expr = template.body): For = {
    val res = For(inits, cond, steps, body)
    copyProperties(template, res)
    res
  }

  def copyBlock(template: Block)(stmts: List[Tree] =
    template.stmts): Block = {
    val res = Block(stmts)
    copyProperties(template, res)
    res
  }

  def copyTernary(template: Ternary)(cond: Expr = template.cond,
    thenp: Expr = template.thenp,
    elsep: Expr = template.elsep): Ternary = {
    val res = Ternary(cond, thenp, elsep)
    copyProperties(template, res)
    res
  }


  def copyApply(template: Apply)(fun: Expr = template.fun,
    args: List[Expr] = template.args): Apply = {

    val res = Apply(fun, args)
    copyProperties(template, res)
    res
  }

  def copyReturn(template: Return)(expr: Option[Expr] =
      template.expr): Return = {
    val res = Return(expr)
    copyProperties(template, res)
    res
  }


  def copyMethodDef(template: MethodDef)(ret: UseTree = template.ret,
    name: Name = template.name, params: List[ValDef]  = template.params,
    body: Expr = template.body): MethodDef = {
    val res = MethodDef(ret, name, params, body)
    copyProperties(template, res)
    res
  }

  def copyValDef(template: ValDef)(mods: Flags = template.mods,
    tpt: UseTree = template.tpt, name: Name = template.name,
    rhs: Expr = template.rhs): ValDef = {
    val res = ValDef(mods, tpt, name, rhs)
    copyProperties(template, res)
    res
  }
}

object TreeCopiers extends TreeCopiers


