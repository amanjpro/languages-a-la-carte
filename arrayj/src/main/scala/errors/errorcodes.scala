package ch.usi.inf.l3.sana.arrayj.errors

import ch.usi.inf.l3.sana
import sana.tiny.errors.ErrorCode
import sana.brokenj


trait ErrorCodes extends brokenj.errors.ErrorCodes {
  case object ARRAY_INDEX_NOT_INT extends ErrorCode {
    val message: String =
      "Array index must be of type int"
  }


  case object ARRAY_SIZE_NOT_INT extends ErrorCode {
    val message: String =
      "Array size must be of type int"
  }

  case object NON_ARRAY_ELEMENT_ACCESS extends ErrorCode {
    val message: String =
      "Only array expressions can be accessed by index"
  }
}

object ErrorCodes extends ErrorCodes
