package ch.usi.inf.l3.sana.modulej.errors

import ch.usi.inf.l3.sana
import sana.tiny.errors.ErrorCode
import sana.dynj


trait ErrorCodes extends dynj.errors.ErrorCodes {
  case object IMPORTED_PACKAGE_IS_MISSING extends ErrorCode {
    val message: String =
      "Package cannot be found"
  }

  case object IMPORTED_CLASS_IS_MISSING extends ErrorCode {
    val message: String =
      "Class cannot be found"
  }


  case object NATIVE_METHOD_CANNOT_HAVE_BODY extends ErrorCode {
    val message: String = "Native methods must not have body"
  }

  case object CONSTRUCTOR_CANNOT_BE_NATIVE extends ErrorCode {
    val message: String = "Constructors cannot be native"
  }
}

object ErrorCodes extends ErrorCodes

