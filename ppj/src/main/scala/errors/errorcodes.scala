package ch.usi.inf.l3.sana.ppj.errors

import ch.usi.inf.l3.sana
import sana.tiny.errors.ErrorCode
import sana.dynj


trait ErrorCodes extends dynj.errors.ErrorCodes {
  case object REFERENCE_TYPE_EXPECTED extends ErrorCode {
    val message: String =
      "An expression of a reference type expected here"
  }
}

object ErrorCodes extends ErrorCodes

