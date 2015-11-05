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



trait TreeFactories extends sana.tiny.ast.TreeFactories {

  def mkCast(tpt: UseTree, expr: Expr,
           pos: Option[Position] = None): Cast = {
    val res = Cast(tpt, expr)
    tpt.tpe.foreach(res.tpe = _)
    expr.owner.foreach(res.owner = _)
    pos.foreach(res.pos = _)
    res
  }


  def mkLiteral(constant: Constant,
              pos: Option[Position] = None,
              owner: Option[Symbol] = None): Literal = {
    val res = Literal(constant)
    res.tpe = constant.tpe
    owner.foreach(res.owner = _)
    pos.foreach(res.pos = _)
    res
  }


  def mkBinary(lhs: Expr, op: BOp, rhs: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): Binary = {
    val res = Binary(lhs, op, rhs)
    owner.foreach(res.owner = _)
    tpe.foreach(res.tpe = _)
    pos.foreach(res.pos = _)
    res
  }

  def mkUnary(isPostfix: Boolean, op: UOp, expr: Expr,
              pos: Option[Position] = None,
              tpe: Option[Type]     = None,
              owner: Option[Symbol] = None): Unary = {
    val res = Unary(isPostfix, op, expr)
    owner.foreach(res.owner = _)
    tpe.foreach(res.tpe = _)
    pos.foreach(res.pos = _)
    res
  }

}

object TreeFactories extends TreeFactories
