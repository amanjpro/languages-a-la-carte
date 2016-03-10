package ch.usi.inf.l3.sana.robustj.errors

import ch.usi.inf.l3.sana
import sana.tiny.errors.ErrorCode
import sana.arrooj


trait ErrorCodes extends arrooj.errors.ErrorCodes {
  case object THROWING_NON_THROWABLE extends ErrorCode {
    val message: String =
      "The thrown expression must be of type java.lang.Throwable"
  }

  case object CATCHING_NON_THROWABLE extends ErrorCode {
    val message: String =
      "Catch parameter must be of type java.lang.Throwable"
  }

  case object NON_THROWABLE_IN_THROWS_CLAUSE extends ErrorCode {
    val message: String =
      "Incompatible types"
  }


  case object NO_CATCH_FOUND extends ErrorCode {
    val message: String =
      "``try'' statements should catches at least one exception"
  }

  case object UNREPORTED_EXCEPTION extends ErrorCode {
    val message: String =
      "Unreported exceptionforeach"
  }

  case object HANDLING_NON_THROWN_EXCEPTION extends ErrorCode {
    val message: String =
      "Exception is never thrown"
  }
}

object ErrorCodes extends ErrorCodes

