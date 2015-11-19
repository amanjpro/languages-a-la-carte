package ch.usi.inf.l3.sana.tiny.source

import org.antlr.v4.runtime.tree.ParseTree

case class SourceFile(name: String, content: ParseTree) {
  private[this] lazy val files: List[String] =
    name.split("[.]").reverse.toList match {
      case "java"::xs         => xs
      case xs                 => xs
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

