package ch.usi.inf.l3.sana.dynj.ast


import ch.usi.inf.l3.sana.calcj
import calcj.ast.operators.BOp

trait operators {
  object InstanceOf extends BOp {
    override def toString: String = "instanceof"
  }
}

object operators extends operators
