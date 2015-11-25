package ch.usi.inf.l3.sana.calcj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj

import tiny.ast._
import tiny.types._
import tiny.source.Position
import tiny.symbols.Symbol

import calcj.types._
import operators._




trait CastApi extends Expr {
  def tpt: UseTree
  def expr: Expr
}

trait LiteralApi extends Expr {
  def constant: Constant
}

trait BinaryApi extends Expr {
  def lhs: Expr
  def op: BOp
  def rhs: Expr
}

trait UnaryApi extends Expr {
  def isPostfix: Boolean
  def op: UOp
  def expr: Expr
}



protected[ast] class Cast(val tpt: UseTree,
  val expr: Expr) extends CastApi {
  override def toString: String =
    s"Cast(${tpt.toString}, ${expr.toString})"
}


protected[ast] class Literal(val constant: Constant) extends LiteralApi {
  override def toString: String =
    s"Literal(${constant.toString})"
}


protected[ast] class Binary(val lhs: Expr,
  val op: BOp, val rhs: Expr) extends BinaryApi {
  override def toString: String =
    s"Binary(${lhs.toString}, ${op.toString}, ${rhs.toString})"
}

protected[ast] class Unary(val isPostfix: Boolean,
  val op: UOp, val expr: Expr) extends UnaryApi {
  override def toString: String =
    s"Unary(${isPostfix.toString}, ${op.toString}, ${expr.toString})"
}

