package ch.usi.inf.l3.sana.arrooj.errors

import ch.usi.inf.l3.sana
import sana.tiny.errors.ErrorCode
import sana.ooj
import sana.arrayj


trait ErrorCodes extends ooj.errors.ErrorCodes {
  val ARRAY_INDEX_NOT_INT: ErrorCode =
    arrayj.errors.ErrorCodes.ARRAY_INDEX_NOT_INT

  val ARRAY_SIZE_NOT_INT: ErrorCode =
    arrayj.errors.ErrorCodes.ARRAY_SIZE_NOT_INT

  val NON_ARRAY_ELEMENT_ACCESS: ErrorCode =
    arrayj.errors.ErrorCodes.NON_ARRAY_ELEMENT_ACCESS
}

object ErrorCodes extends ErrorCodes

