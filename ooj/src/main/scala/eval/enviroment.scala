package ch.usi.inf.l3.sana.ooj.eval


import ch.usi.inf.l3.sana
import sana.tiny.symbols.Symbol
import sana.tiny.ast.Expr
import sana.calcj.ast.LiteralApi

trait Value
case class ExprValue(value: Expr) extends Value
case class LiteralValue(value: LiteralApi) extends Value

trait Env {
  protected val bindings: Map[Symbol, Value]

  def bind(symbol: Symbol, value: Value): Env = {
    val newBindings = bindings + (symbol -> value)
    create(newBindings)
  }


  protected def defines(symbol: Symbol): Boolean =
    bindings.contains(symbol)

  def getValue(symbol: Symbol): Option[Value] =
    bindings.get(symbol)

  def unbind(symbol: Symbol): Env = {
    val newBindings = bindings - symbol
    create(newBindings)
  }

  protected def create(bindings: Map[Symbol, Value]): Env


  override def hashCode: Int = bindings.hashCode
  override def equals(other: Any): Boolean = other match {
    case null      =>
      false
    case that: Env =>
      bindings == that.bindings
    case _         =>
      false
  }
}


object Env {
  private class EnvImpl(
      protected val bindings: Map[Symbol, Value]) extends Env {
    protected def create(bindings: Map[Symbol, Value]): Env =
      new EnvImpl(bindings)
  }

  def apply(bindings: Map[Symbol, Value]): Env =
    new EnvImpl(bindings)

  def emptyEnv: Env =
    apply(Map.empty)
}
