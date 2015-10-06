package ch.usi.inf.l3.sana.brokenj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import tiny.ast.{Tree, Expr, NamedTree}
import tiny.source.Position
import tiny.types.Type
import tiny.symbols.Symbol
import tiny.names.Name
import tiny.modifiers.Flags
import primj.types.VoidType


/********************* AST Nodes *********************************/

trait LabelApi extends Expr with NamedTree {
  def name: Name
  def stmt: Expr
  val symbol: Option[Symbol] = None
  val tpe: Option[Type] = stmt.tpe
}

trait BreakApi extends Expr {
  def label: Option[Name]
  val tpe: Option[Type] = Some(VoidType)
  val symbol: Option[Symbol] = None
}

trait ContinueApi extends Expr {
  def label: Option[Name]
  val tpe: Option[Type] = Some(VoidType)
  val symbol: Option[Symbol] = None
}

trait CaseApi extends Tree {
  def guards: List[Expr]
  def body: Tree
  def tpe: Option[Type] = Some(VoidType)
  val symbol: Option[Symbol] = None
}

// trait DefaultCaseApi extends CaseApi {
//   def pos: Option[Position] = None
// }

trait SwitchApi extends Expr {
  def expr: Expr
  def cases: List[CaseApi]

  val tpe: Option[Type] = Some(VoidType)
}


case class Label(name: Name, stmt: Expr, pos: Option[Position],
  owner: Option[Symbol]) extends LabelApi

case class Break(label: Option[Name], pos: Option[Position],
  owner: Option[Symbol]) extends BreakApi

case class Continue(label: Option[Name], pos: Option[Position],
  owner: Option[Symbol]) extends ContinueApi

case class Case(guards: List[Expr], body: Tree,
  pos: Option[Position], owner: Option[Symbol]) extends CaseApi

case class Switch(expr: Expr, cases: List[CaseApi],
  pos: Option[Position], owner: Option[Symbol]) extends SwitchApi

// case class DefaultCase() extends DefaultCaseApi
// /***************************** Extractors **************************/
//
// trait LabelExtractor {
//   def unapply(lbl: Label): Option[(Name, Expr)] = lbl match {
//     case null     => None
//     case _        => Some((lbl.name, lbl.stmt))
//   }
// }
//
// trait SwitchExtractor {
//   def unapply(switch: Switch): Option[(Expr, List[Case])] =
//     switch match {
//       case null     => None
//       case _        => Some((switch.expr, switch.cases))
//     }
// }
//
//
// trait CaseExtractor {
//   def unapply(cse: Case): Option[(List[Expr], Tree)] = cse match {
//     case null     => None
//     case _        => Some((cse.guards, cse.body))
//   }
// }
//
// trait BreakExtractor {
//   def unapply(brk: Break): Option[Option[Name]] = brk match {
//     case null     => None
//     case _        => Some(brk.label)
//   }
// }
//
// trait ContinueExtractor {
//   def unapply(cnt: Continue): Option[Option[Name]] = cnt match {
//     case null     => None
//     case _        => Some(cnt.label)
//   }
// }
//
// /***************************** Factories **************************/
//
// trait LabelFactory {
//   private class LabelImpl(val name: Name, val stmt: Expr,
//     val pos: Option[Position], val owner: TreeId) extends Label
//
//   def apply(name: Name, stmt: Expr,
//     pos: Option[Position], owner: TreeId): Label =
//     new LabelImpl(name, stmt, pos, owner)
// }
//
//
// trait SwitchFactory {
//   private class SwitchImpl(val expr: Expr,
//     val cases: List[Case], val pos: Option[Position],
//     val owner: TreeId) extends Switch
//
//
//   def apply(expr: Expr, cases: List[Case],
//       pos: Option[Position], owner: TreeId): Switch =
//     new SwitchImpl(expr, cases, pos, owner)
// }
//
// trait CaseFactory {
//   private class CaseImpl(val guards: List[Expr],
//     val body: Tree,
//     val pos: Option[Position], val owner: TreeId) extends Case
//
//
//   def apply(guards: List[Expr], body: Tree,
//       pos: Option[Position], owner: TreeId): Case =
//     new CaseImpl(guards, body, pos, owner)
// }
//
//
// trait BreakFactory {
//   private class BreakImpl(val label: Option[Name],
//     val pos: Option[Position], val owner: TreeId) extends Break
//
//   def apply(label: Option[Name],
//     pos: Option[Position], owner: TreeId): Break =
//     new BreakImpl(label, pos, owner)
// }
//
// trait ContinueFactory {
//   private class ContinueImpl(val label: Option[Name],
//     val pos: Option[Position], val owner: TreeId) extends Continue
//
//   def apply(label: Option[Name],
//     pos: Option[Position], owner: TreeId): Continue =
//     new ContinueImpl(label, pos, owner)
// }


  /******************* Factory and Extractor instances ***************/


//   // TODO: Only let Extractors out, or none?
//
//   val Label       = new LabelExtractor with LabelFactory {}
//   val Switch      = new SwitchExtractor with SwitchFactory {}
//   val Case        = new CaseExtractor with CaseFactory {}
//   val Break       = new BreakExtractor with BreakFactory {}
//   val Continue    = new ContinueExtractor with ContinueFactory {}
// }
