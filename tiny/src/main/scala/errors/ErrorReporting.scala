package ch.usi.inf.l3.sana.tiny.errors

import ch.usi.inf.l3.sana
import sana.tiny.source.Position
import ErrorCodes._


object ErrorReporting {
  def errors: Vector[Report] = messages
  private[this] var messages: Vector[Report] = Vector()

  private[this] var _isTest: Option[Boolean] = None
  def isTest: Boolean = _isTest.getOrElse(false)
  def isTest_=(b: Boolean): Unit = _isTest match {
    case None       => _isTest = Some(b)
    case s          => ()
  }


  def isErroneous(): Boolean =
    messages.filter(_.isError) != Vector.empty



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



  def error(code: ErrorCode, found: String, required: String,
    pos: Option[Position]): Unit = {
      messages = messages :+ Report(Error,
        createMessageOrGetCode(code, found, required, pos),
        isTest)
  }

  def warning(code: ErrorCode, found: String, required: String,
    pos: Option[Position]): Unit = {
      messages = messages :+ Report(Warning,
        createMessageOrGetCode(code, found, required, pos),
        isTest)
  }
}
