package ch.usi.inf.l3.sana.dynj.errors

import ch.usi.inf.l3.sana
import sana.tiny.errors.ErrorCode
import sana.robustj


trait ErrorCodes extends robustj.errors.ErrorCodes {
  case object DYNAMIC_TYPE_CHECK_NONE_REF_TYPE extends ErrorCode {
    val message: String =
      "Dynamic type checks can only be performed on reference types"
  }
}

object ErrorCodes extends ErrorCodes
