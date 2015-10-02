package ch.usi.inf.l3.sana.tiny.parsers

import ch.usi.inf.l3.sana.tiny
import tiny.ast._
import tiny.source.{SourceFile, Position}

trait Parser {
  def parse(source: SourceFile): Tree
}
