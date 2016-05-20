package ch.usi.inf.l3.sana.dynj.ast


import ch.usi.inf.l3.sana.calcj
import calcj.ast.operators.BOp

object operators {
  object InstanceOf extends BOp {
    override def toString: String = "instanceof"
  }
}
