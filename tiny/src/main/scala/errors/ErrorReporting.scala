/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
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

package ch.usi.inf.l3.sana.tiny.errors

import ch.usi.inf.l3.sana
import sana.tiny.source.Position
import ErrorCodes._


/**
 * A module for error reporting
 */
object ErrorReporting {

  /** A list of reported errors and warnings */
  def errors: Vector[Report] = messages

  private[this] var messages: Vector[Report] = Vector()

  /** Are we in testing mode or production mode */
  private[this] var _isTest: Option[Boolean] = None
  def isTest: Boolean = _isTest.getOrElse(false)
  def isTest_=(b: Boolean): Unit = _isTest match {
    case None       => _isTest = Some(b)
    case s          => ()
  }


  /** Returns true if the compilation process is erroneous */
  def isErroneous(): Boolean =
    !messages.filter(_.isError).isEmpty



  protected def createMessage(code: ErrorCode, found: String,
    required: String, pos: Option[Position]): String = {
      val msg = code.message
      val col = pos match {
        case None    => 0
        case Some(p) => 4 + p.col
      }
      val caret = if(col != 0) {
        (" " * col) + "^"
      } else ""
      val source = pos.map(_.source).getOrElse("")
      val row    = pos.map(_.row.toString).getOrElse("")
      val c      = pos.map(_.col.toString).getOrElse("")
      val line   = pos.map(_.line).getOrElse("")
      s"""|Source: ${source}, Line: ${row}, Column: ${c}
      |$msg
      |${" " * 2}$found
      |${" " * 2}$required
      |${"     "}${line}
      |$caret""".stripMargin
  }



  protected def createMessageOrGetCode(code: ErrorCode, found: String,
    required: String, pos: Option[Position]): String =
      if(isTest) code.code
      else
        createMessage(code, found, required, pos)

  def genError(code: ErrorCode, found: String, required: String,
    pos: Option[Position]): Report = Report(Error,
      createMessageOrGetCode(code, found, required, pos),
      isTest)

  def genWarning(code: ErrorCode, found: String, required: String,
    pos: Option[Position]): Report =
      Report(Warning,
        createMessageOrGetCode(code, found, required, pos),
        isTest)



  /**
   * Report an error
   *
   * @param code the error code of this error
   * @param found the actual result
   * @param required the expected result
   * @param pos the position of the erroneous AST
   */
  def error(code: ErrorCode, found: String, required: String,
    pos: Option[Position]): Unit = {
      messages = messages :+ Report(Error,
        createMessageOrGetCode(code, found, required, pos),
        isTest)
  }

  /**
   * Report a warning
   *
   * @param code the error code of thi warning
   * @param found the actual result
   * @param required the expected result
   * @param pos the position of the erroneous AST
   */
  def warning(code: ErrorCode, found: String, required: String,
    pos: Option[Position]): Unit = {
      messages = messages :+ Report(Warning,
        createMessageOrGetCode(code, found, required, pos),
        isTest)
  }
}
