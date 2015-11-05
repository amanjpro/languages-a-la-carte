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
}

trait BreakApi extends Expr {
  def label: Option[Name]
}

trait ContinueApi extends Expr {
  def label: Option[Name]
}

trait CaseApi extends Tree {
  def guards: List[Expr]
  def body: Tree
}

// trait DefaultCaseApi extends CaseApi {
//   def pos: Option[Position] = None
// }

trait SwitchApi extends Expr {
  def expr: Expr
  def cases: List[CaseApi]
}


case class Label protected[ast](name: Name, stmt: Expr) extends LabelApi

case class Break protected[ast](label: Option[Name]) extends BreakApi

case class Continue protected[ast](label: Option[Name]) extends ContinueApi

case class Case protected[ast](guards: List[Expr],
  body: Tree) extends CaseApi

case class Switch protected[ast](expr: Expr,
  cases: List[CaseApi]) extends SwitchApi

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
