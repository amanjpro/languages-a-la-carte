package ch.usi.inf.l3.sana.calcj.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.tiny.ast.Implicits._
import sana.tiny.ast._
import sana.calcj.ast._
import operators._



trait TreeCopiers extends sana.tiny.ast.TreeCopiers {

  def copyCast(template: CastApi)(
      tpt: UseTree = template.tpt,
      expr: Expr = template.expr): CastApi = {
    val res = TreeFactories.mkCast(tpt, expr)
    copyProperties(template, res)
    res
  }


  def copyLiteral(template: LiteralApi)
      (constant: Constant): LiteralApi = {
    val res = TreeFactories.mkLiteral(constant)
    copyProperties(template, res)
    res
  }


  def copyBinary(template: BinaryApi)(lhs: Expr = template.lhs,
      op: BOp = template.op, rhs: Expr = template.rhs): BinaryApi = {
    val res = TreeFactories.mkBinary(lhs, op, rhs)
    copyProperties(template, res)
    res
  }

  def copyUnary(template: UnaryApi)(isPostfix: Boolean = template.isPostfix,
    op: UOp = template.op, expr: Expr = template.expr): UnaryApi = {
    val res = TreeFactories.mkUnary(isPostfix, op, expr)
    copyProperties(template, res)
    res
  }

}

object TreeCopiers extends TreeCopiers
