/*
 * Copyright (c) <2015-2016>, see CONTRIBUTERS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package ch.usi.inf.l3.sana.primj.errors

import ch.usi.inf.l3.sana.tiny
import tiny.errors.ErrorCode

trait ErrorCodes extends tiny.errors.ErrorCodes {
  case object DOUBLE_DEF extends ErrorCode {
    val message: String = "Name is already defined"
  }
  case object NAME_NOT_FOUND extends ErrorCode {
    val message: String = "Name not found"
  }

  case object VOID_VARIABLE_TYPE extends ErrorCode {
    val message: String = "Void type variable"
  }

  case object UNINITIALIZED_FINAL_VARIABLE extends ErrorCode {
    val message: String = "Final variable is uninitialized"
  }

  case object TYPE_NOT_FOUND extends ErrorCode {
    val message: String = "Type not found"
  }


  case object REASSIGNING_FINAL_VARIABLE extends ErrorCode {
    val message: String = "Reassigining to a final variable"
  }

  case object ASSIGNING_NOT_TO_VARIABLE extends ErrorCode {
    val message: String = "Assigning to a tree that is not a variable"
  }


  case object MISSING_RETURN_STATEMENT extends ErrorCode {
    val message: String = "Missing return statement"
  }

  case object VOID_RETURN extends ErrorCode {
    val message: String = "Missing expression"
  }

  case object NON_VOID_RETURN extends ErrorCode {
    val message: String = "Return cannot have an expression here"
  }

  case object TYPE_NAME_EXPECTED extends ErrorCode {
    val message: String = "A type name is expected"
  }

  case object VARIABLE_ALREADY_DEFINED extends
      ErrorCode {
    val message: String =
      "Variable already defined"
  }

  case object METHOD_ALREADY_DEFINED extends
      ErrorCode {
    val message: String =
      "Method already defined"
  }

  case object PARAM_OWNED_BY_NON_METHOD extends ErrorCode {
    val message: String =
      "Parameter can only appear in method parameter contexts"
  }

}

object ErrorCodes extends ErrorCodes
