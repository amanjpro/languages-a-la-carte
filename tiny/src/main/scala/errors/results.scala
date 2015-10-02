package ch.usi.inf.l3.sana.tiny.errors

case class Report(kind: ReportKind, message: String, isTest: Boolean) {
  def isError: Boolean = kind == Error

  override def toString: String =
    if(isTest) message
    else s"$kind: $message"
}


trait ReportKind
case object Error extends ReportKind {
  override def toString: String = "[error]"
}
case object Warning extends ReportKind {
  override def toString: String = "[warning]"
}
