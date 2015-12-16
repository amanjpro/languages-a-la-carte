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

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = members.foldLeft(z)(f)
    f(r1, this)
  }
}

// Variable and Method definitions
trait MethodDefApi extends TermTree {
  def ret: UseTree
  def params: List[ValDefApi]
  def body: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, ret)
    val r2 = params.foldLeft(r1)(f)
    val r3 = f(r2, body)
    f(r3, this)
  }
}

// At this stage, methods don't have modifiers (no encapsulation,
// no final). But ValDefs have, since they can be either params, normal
// variables or final variables.
trait ValDefApi extends TermTree {
  def mods: Flags
  def tpt: UseTree
  def rhs: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, tpt)
    val r2 = f(r1, rhs)
    f(r2, this)
  }
}



trait ReturnApi extends Expr {
  val expr: Option[Expr]
  def isVoid: Boolean = expr == None

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = expr.map(f(z, _)).getOrElse(z)
    f(z, this)
  }
}

trait BlockApi extends Expr {
  def stmts: List[Tree]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = stmts.foldLeft(z)(f)
    f(r1, this)
  }

}



trait AssignApi extends Expr {
  def lhs: Expr
  def rhs: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, lhs)
    val r2 = f(r1, rhs)
    f(r2, this)
  }
}

trait IfApi extends Expr {
  def cond: Expr
  def thenp: Expr
  def elsep: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, cond)
    val r2 = f(r1, thenp)
    val r3 = f(r2, elsep)
    f(r3, this)
  }

}


trait WhileApi extends Expr {
  def isDoWhile: Boolean
  def cond: Expr
  def body: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, cond)
    val r2 = f(r1, body)
    f(r2, this)
  }
}

trait ForApi extends Expr {
  def inits: List[Tree]
  def cond: Expr
  def steps: List[Expr]
  def body: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = inits.foldLeft(z)(f)
    val r2 = f(r1, cond)
    val r3 = steps.foldLeft(r2)(f)
    val r4 = f(r3, body)
    f(r4, this)
  }
}

// Ternary operator
trait TernaryApi extends Expr {
  def cond: Expr
  def thenp: Expr
  def elsep: Expr

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, cond)
    val r2 = f(r1, thenp)
    val r3 = f(r2, elsep)
    f(r3, this)
  }
}

// Apply
trait ApplyApi extends Expr {
  def fun: Expr
  def args: List[Expr]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, fun)
    val r2 = args.foldLeft(r1)(f)
    f(r2, this)
  }
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
