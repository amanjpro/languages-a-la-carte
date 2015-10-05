package ch.usi.inf.l3.sana.tiny.errors

trait ErrorCode {
  def message: String
  lazy val code: String = this.toString
}

trait ErrorCodes {
  case object BAD_STATEMENT extends ErrorCode {
    val message: String = "Unexpected statement here"
  }
  case object TYPE_MISMATCH extends ErrorCode {
    val message: String = "Type mismatch"
  }
  case object UNEXPETED_TREE extends ErrorCode {
    val message: String = "Unexpected tree"
  }

  case object BAD_EXPRESSION extends ErrorCode {
    val message: String = "Expected an expression here"
  }
}

object ErrorCodes extends ErrorCodes

