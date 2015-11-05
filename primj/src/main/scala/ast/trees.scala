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
  def params: List[ValDef]
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

case class Program protected[ast](members: List[DefTree],
  sourceName: String) extends ProgramApi

case class Assign protected[ast](lhs: Expr, rhs: Expr) extends AssignApi

case class If protected[ast](cond: Expr, thenp: Expr,
  elsep: Expr) extends IfApi


case class While protected[ast](isDoWhile: Boolean, cond: Expr,
  body: Expr) extends WhileApi

case class Block protected[ast](stmts: List[Tree]) extends BlockApi

case class For protected[ast](inits: List[Tree],
  cond: Expr, steps: List[Expr], body: Expr) extends ForApi

case class Ternary protected[ast](cond: Expr, thenp: Expr,
  elsep: Expr) extends TernaryApi

case class Apply protected[ast](fun: Expr, args: List[Expr]) extends ApplyApi

case class Return protected[ast](expr: Option[Expr]) extends ReturnApi

case class MethodDef protected[ast](ret: UseTree,
  name: Name, params: List[ValDef],
  body: Expr) extends MethodDefApi

case class ValDef protected[ast](mods: Flags,
  tpt: UseTree, name: Name, rhs: Expr) extends ValDefApi
