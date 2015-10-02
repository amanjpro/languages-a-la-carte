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


trait ProgramApi extends SymTree {
  /**
    * List of definitions defined in this template.
    */
  def members: List[DefTree]
  val tpe: Option[Type] = None
  def pos: Option[Position] = None
  def sourceName: String
  override def owner: Option[Symbol] = None
}

// Variable and Method definitions
trait MethodDefApi extends TermTree {
  def ret: UseTree
  def params: List[ValDef]
  def body: Expr
  val tpe: Option[Type] = {
    val tys = params.flatMap(_.tpe)
    for {
      r   <- ret.tpe
    } yield MethodType(r, tys)
  }
}

// At this stage, methods don't have modifiers (no encapsulation,
// no final). But ValDefs have, since they can be either params, normal
// variables or final variables.
trait ValDefApi extends TermTree {
  def mods: Flags
  def tpt: UseTree
  def rhs: Expr
  val tpe: Option[Type] = tpt.tpe
}



trait ReturnApi extends Expr {
  val expr: Option[Expr]
  val tpe: Option[Type] = expr.map(_.tpe).getOrElse(Some(VoidType))

  def isVoid: Boolean = expr == None
}

trait BlockApi extends Expr with SymTree {
  def stmts: List[Tree]

  val tpe: Option[Type] = stmts match {
    case Nil =>
      Some(VoidType)
    case _   =>
      stmts.last.tpe
  }
}



trait AssignApi extends Expr {
  def lhs: Expr
  def rhs: Expr
  val tpe: Option[Type] = lhs.tpe
}

trait IfApi extends Expr {
  def cond: Expr
  def thenp: Expr
  def elsep: Expr
  val tpe: Option[Type] = Some(VoidType)
}


trait WhileApi extends Expr {
  def isDoWhile: Boolean
  def cond: Expr
  def body: Expr
  val tpe: Option[Type] = Some(VoidType)
}

trait ForApi extends Expr with SymTree {
  def inits: List[Tree]
  def cond: Expr
  def steps: List[Expr]
  def body: Expr
  val tpe: Option[Type] = Some(VoidType)
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
  val tpe: Option[Type] = fun.tpe match {
    case Some(MethodType(r, _)) => Some(r)
    case _                      => None
  }


}

case class Program(members: List[DefTree],
  symbol: Option[Symbol], sourceName: String) extends ProgramApi

case class Assign(lhs: Expr,
  rhs: Expr, pos: Option[Position],
  owner: Option[Symbol]) extends AssignApi

case class If(cond: Expr, thenp: Expr,
  elsep: Expr, pos: Option[Position],
  owner: Option[Symbol]) extends IfApi


case class While(isDoWhile: Boolean, cond: Expr,
  body: Expr, pos: Option[Position],
  owner: Option[Symbol]) extends WhileApi

case class Block(stmts: List[Tree],
  pos: Option[Position], symbol: Option[Symbol],
  owner: Option[Symbol]) extends BlockApi

case class For(inits: List[Tree],
  cond: Expr, steps: List[Expr], body: Expr,
  pos: Option[Position], symbol: Option[Symbol],
  owner: Option[Symbol]) extends ForApi

case class Ternary(cond: Expr, thenp: Expr,
  elsep: Expr, tpe: Option[Type],
  pos: Option[Position], owner: Option[Symbol]) extends TernaryApi

case class Apply(fun: Expr, args: List[Expr],
  pos: Option[Position], owner: Option[Symbol]) extends ApplyApi

case class Return(expr: Option[Expr],
  pos: Option[Position], owner: Option[Symbol]) extends ReturnApi

case class MethodDef(ret: UseTree,
  name: Name, params: List[ValDef],
  body: Expr, pos: Option[Position],
  symbol: Option[Symbol], owner: Option[Symbol]) extends MethodDefApi

case class ValDef(mods: Flags,
  tpt: UseTree, name: Name, rhs: Expr,
  pos: Option[Position], symbol: Option[Symbol],
  owner: Option[Symbol]) extends ValDefApi
