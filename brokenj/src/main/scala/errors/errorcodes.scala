package ch.usi.inf.l3.sana.brokenj.errors

import ch.usi.inf.l3.sana
import sana.tiny.errors.ErrorCode
import sana.primj


trait ErrorCodes extends primj.errors.ErrorCodes {
  case object DOUBLE_LABEL_DEF extends ErrorCode {
    val message: String = "Label is already defined"
  }

  case object NO_LABEL_DEF extends ErrorCode {
    val message: String = "Label not found"
  }

  case object BAD_CONTINUE_STMT extends ErrorCode {
    val message: String = "Continue can only appear in iterative statements"
  }

  case object BAD_BREAK_STMT extends ErrorCode {
    val message: String = "Break can only appear in breakable statements"
  }

  case object NOT_DISTINCT_GUARD extends ErrorCode {
    val message: String = "Case guard is not distinct"
  }

  case object CASE_GUARD_NOT_CONSTANT_EXPRESSION extends ErrorCode {
    val message: String = "Case guard is not constant expression"
  }
}

object ErrorCodes extends ErrorCodes

