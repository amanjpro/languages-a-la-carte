package ch.usi.inf.l3.sana.dynj.ast


import ch.usi.inf.l3.sana.calcj
import calcj.ast.operators.BOp

trait operators extends calcj.ast.operators {
  object IsInstanceOf extends BOp {
    override def toString: String = "isinstanceof"
  }
}

object operators extends operators
