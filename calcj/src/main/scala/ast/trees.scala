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



case class Cast protected[ast](tpt: UseTree, expr: Expr) extends CastApi


case class Literal protected[ast](constant: Constant) extends LiteralApi


case class Binary protected[ast](lhs: Expr, op: BOp, rhs: Expr) extends BinaryApi

case class Unary protected[ast](isPostfix: Boolean,
  op: UOp, expr: Expr) extends UnaryApi

