package ch.usi.inf.l3.sana.brokenj.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import sana.primj.ast.Implicits._
import sana.tiny.modifiers.Flags
import sana.tiny.ast._
import sana.calcj.ast._
import sana.primj.ast._
import sana.primj.types._


trait TreeFactories extends sana.primj.ast.TreeFactories {

  def mkLabel(name: Name, stmt: Expr,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): LabelApi = {
    val res = new Label(name, stmt)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    stmt.tpe.foreach(res.tpe = _)
    res
  }

  def mkBreak(label: Option[Name],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): BreakApi = {
    val res = new Break(label)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res.tpe = VoidType
    res
  }

  def mkContinue(label: Option[Name],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): ContinueApi = {
    val res = new Continue(label)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res.tpe = VoidType
    res
  }

  def mkCase(guards: List[Expr], body: Tree,
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): CaseApi = {
    val res = new Case(guards, body)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res.tpe = VoidType
    res
  }

  def mkSwitch(expr: Expr, cases: List[CaseApi],
    pos: Option[Position] = None,
    owner: Option[Symbol] = None): SwitchApi = {
    val res = new Switch(expr, cases)
    pos.foreach(res.pos = _)
    owner.foreach(res.owner = _)
    res.tpe = VoidType
    res
  }
}


object TreeFactories extends TreeFactories
