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
  def tpe   = tpt.tpe
}

trait LiteralApi extends Expr {
  def constant: Constant
  def tpe = Some(constant.tpe)
  def owner = None
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



case class Cast(tpt: UseTree, expr: Expr,
  pos: Option[Position],
  owner: Option[Symbol]) extends CastApi


case class Literal(constant: Constant,
  pos: Option[Position]) extends LiteralApi


case class Binary(lhs: Expr, op: BOp, rhs: Expr,
  tpe: Option[Type], owner: Option[Symbol], pos: Option[Position])
  extends BinaryApi

case class Unary(isPostfix: Boolean, op: UOp, expr: Expr,
  tpe: Option[Type], owner: Option[Symbol], pos: Option[Position])
  extends UnaryApi

