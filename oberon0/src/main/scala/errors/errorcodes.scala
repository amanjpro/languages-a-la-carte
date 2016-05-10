package ch.usi.inf.l3.sana.oberon0.errors

import ch.usi.inf.l3.sana
import sana.tiny.errors.ErrorCode
import sana.arrooj


trait ErrorCodes extends arrooj.errors.ErrorCodes {
  object MODULE_ALREADY_DEFINED extends ErrorCode {
    def message: String =
      "Module already defined"
  }

  object TYPE_ALREADY_DEFINED extends ErrorCode {
    def message: String =
      "Type already defined"
  }

  object NAME_MISMATCH extends ErrorCode {
    def message: String = "Names do not match"
  }
}

object ErrorCodes extends ErrorCodes

