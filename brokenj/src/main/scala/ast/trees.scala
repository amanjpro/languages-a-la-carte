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

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = stmt.bottomUp(z)(f)
    f(r1, this)
  }
}

trait BreakApi extends Expr {
  def label: Option[Name]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R =
    f(z, this)
}

trait ContinueApi extends Expr {
  def label: Option[Name]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R =
    f(z, this)
}

trait CaseApi extends Tree {
  def guards: List[Expr]
  def body: Tree

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = guards.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r2 = body.bottomUp(r1)(f)
    f(r2, this)
  }
}

// trait DefaultCaseApi extends CaseApi {
//   def pos: Option[Position] = None
// }

trait SwitchApi extends Expr {
  def expr: Expr
  def cases: List[CaseApi]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = expr.bottomUp(z)(f)
    val r2 = cases.foldLeft(r1)((z, y) => {
      y.bottomUp(z)(f)
    })
    f(r2, this)
  }
}


protected[ast] class Label(val name: Name, val stmt: Expr) extends LabelApi {
  override def toString: String =
    s"Label($name, $stmt)"
}

protected[ast] class Break(val label: Option[Name]) extends BreakApi {
  override def toString: String =
    s"Break($label)"
}

protected[ast] class Continue(val label: Option[Name]) extends ContinueApi {
  override def toString: String =
    s"Continue($label)"
}

protected[ast] class Case(val guards: List[Expr],
  val body: Tree) extends CaseApi {
  override def toString: String =
    s"Case($guards, $body)"
}

protected[ast] class Switch(val expr: Expr,
  val cases: List[CaseApi]) extends SwitchApi {
  override def toString: String =
    s"Switch($expr, $cases)"
}
