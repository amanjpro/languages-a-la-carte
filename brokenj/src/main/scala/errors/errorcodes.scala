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
