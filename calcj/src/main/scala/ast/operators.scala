package ch.usi.inf.l3.sana.calcj.ast


object operators {
  trait Op


  // Unary Operators

  trait UOp extends Op

  trait POp extends Op

  object Not extends UOp {
    override def toString: String = "!"
  }
  object BCompl extends UOp {
    override def toString: String = "~"
  }
  object Inc extends UOp with POp {
    override def toString: String = "++"
  }
  object Dec extends UOp with POp {
    override def toString: String = "--"
  }
  object Neg extends UOp {
    override def toString: String = "-"
  }
  object Pos extends UOp {
    override def toString: String = "+"
  }

  // Binary Operators
  trait BOp extends Op

  object Gt extends BOp {
    override def toString: String = ">"
  }
  object Lt extends BOp {
    override def toString: String = "<"
  }

  object Eq extends BOp {
    override def toString: String = "=="
  }
  object Le extends BOp {
    override def toString: String = "<="
  }
  object Ge extends BOp {
    override def toString: String = ">="
  }
  object Neq extends BOp {
    override def toString: String = "!="
  }
  object And extends BOp {
    override def toString: String = "&&"
  }
  object Or extends BOp {
    override def toString: String = "||"
  }
  object Add extends BOp {
    override def toString: String = "+"
  }
  object Sub extends BOp {
    override def toString: String = "-"
  }
  object Mul extends BOp {
    override def toString: String = "*"
  }
  object Div extends BOp {
    override def toString: String = "/"
  }
  object BAnd extends BOp {
    override def toString: String = "&"
  }
  object BOr extends BOp {
    override def toString: String = "|"
  }
  object BXor extends BOp {
    override def toString: String = "^"
  }
  object Mod extends BOp {
    override def toString: String = "%"
  }
  object SHL extends BOp {
    override def toString: String = "<<"
  }
  object SHR extends BOp {
    override def toString: String = ">>"
  }
  object USHR extends BOp {
    override def toString: String = ">>>"
  }
}
