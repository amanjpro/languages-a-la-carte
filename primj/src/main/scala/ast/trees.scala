package ch.usi.inf.l3.sana.primj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.ast._
import tiny.source.Position
import tiny.types.Type
import tiny.names.Name
import tiny.symbols.Symbol
import tiny.modifiers.Flags
import primj.types._


trait ProgramApi extends Tree {
  /**
    * List of definitions defined in this template.
    */
  def members: List[DefTree]
  def sourceName: String
}

// Variable and Method definitions
trait MethodDefApi extends TermTree {
  def ret: UseTree
  def params: List[ValDefApi]
  def body: Expr
}

// At this stage, methods don't have modifiers (no encapsulation,
// no final). But ValDefs have, since they can be either params, normal
// variables or final variables.
trait ValDefApi extends TermTree {
  def mods: Flags
  def tpt: UseTree
  def rhs: Expr
}



trait ReturnApi extends Expr {
  val expr: Option[Expr]
  def isVoid: Boolean = expr == None
}

trait BlockApi extends Expr {
  def stmts: List[Tree]
}



trait AssignApi extends Expr {
  def lhs: Expr
  def rhs: Expr
}

trait IfApi extends Expr {
  def cond: Expr
  def thenp: Expr
  def elsep: Expr
}


trait WhileApi extends Expr {
  def isDoWhile: Boolean
  def cond: Expr
  def body: Expr
}

trait ForApi extends Expr {
  def inits: List[Tree]
  def cond: Expr
  def steps: List[Expr]
  def body: Expr
}

// Ternary operator
trait TernaryApi extends Expr {
  def cond: Expr
  def thenp: Expr
  def elsep: Expr
}

// Apply
trait ApplyApi extends Expr {
  def fun: Expr
  def args: List[Expr]
}

protected[ast] class Program(val members: List[DefTree],
  val sourceName: String) extends ProgramApi {
  override def toString: String =
    s"Program($members, $sourceName)"
}

protected[ast] class Assign(val lhs: Expr, val rhs: Expr) extends AssignApi {
  override def toString: String =
    s"Assign($lhs, $rhs)"
}

protected[ast] class If(val cond: Expr, val thenp: Expr,
  val elsep: Expr) extends IfApi {
  override def toString: String =
    s"If(${cond.toString}, ${thenp.toString}, ${elsep.toString})"
}


protected[ast] class While(val isDoWhile: Boolean, val cond: Expr,
  val body: Expr) extends WhileApi {
  override def toString: String =
    s"While(${cond.toString}, ${body.toString})"
}

protected[ast] class Block(val stmts: List[Tree]) extends BlockApi {
  override def toString: String =
    s"Block(${stmts.toString})"
}

protected[ast] class For(val inits: List[Tree],
  val cond: Expr, val steps: List[Expr], val body: Expr) extends ForApi {
  override def toString: String =
    s"For($inits, $cond, $steps, $body)"
}

protected[ast] class Ternary(val cond: Expr, val thenp: Expr,
  val elsep: Expr) extends TernaryApi {
  override def toString: String =
    s"Ternary($cond, $thenp, $elsep)"
}

protected[ast] class Apply(val fun: Expr,
  val args: List[Expr]) extends ApplyApi {
  override def toString: String =
    s"Apply($fun, $args)"
}

protected[ast] class Return(val expr: Option[Expr]) extends ReturnApi {
  override def toString: String =
    s"Return($expr)"
}

protected[ast] class MethodDef(val ret: UseTree,
  val name: Name, val params: List[ValDefApi],
  val body: Expr) extends MethodDefApi {
  override def toString: String =
    s"MethodDef($ret, $name, $params, $body)"
}

protected[ast] class ValDef(val mods: Flags,
  val tpt: UseTree, val name: Name, val rhs: Expr) extends ValDefApi {
  override def toString: String =
    s"ValDef($mods, $tpt, $name, $rhs)"
}
