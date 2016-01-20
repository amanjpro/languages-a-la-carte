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

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = tpt.bottomUp(z)(f)
    val r2 = expr.bottomUp(r1)(f)
    f(r2, this)
  }
}

trait LiteralApi extends Expr {
  def constant: Constant

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = f(z, this)
}

trait BinaryApi extends Expr {
  def lhs: Expr
  def op: BOp
  def rhs: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = lhs.bottomUp(z)(f)
    val r2 = rhs.bottomUp(r1)(f)
    f(r2, this)
  }
}

trait UnaryApi extends Expr {
  def isPostfix: Boolean
  def op: UOp
  def expr: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = expr.bottomUp(z)(f)
    f(r1, this)
  }
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

