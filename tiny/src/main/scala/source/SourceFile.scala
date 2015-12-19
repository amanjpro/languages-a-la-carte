package ch.usi.inf.l3.sana.tiny.source

import org.antlr.v4.runtime.tree.ParseTree

case class SourceFile(name: String,
      lines: Array[String], content: ParseTree) {
  private[this] lazy val files: List[String] = {
    name.split("[/|\\\\]").reverse.toList match {
      case x::xs              =>
        x.split("[.]").reverse.toList match {
          case (y::ys)      =>
            ys ++ xs
          case ys           => x::xs
        }
      case Nil                =>
        Nil
    }
  }

  def fileName: String = files match {
    case x::_          => x
    case _             => ""
  }

  def filePath: List[String] = files match {
    case _::xs         => xs
    case _             => Nil
  }
}

