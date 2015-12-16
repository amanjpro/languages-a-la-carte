package ch.usi.inf.l3.sana.tiny.source



import scala.io.Source
import java.io.IOException
import ch.usi.inf.l3.sana.tiny
import java.io.FileInputStream
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._


trait SourceReader {

  def readSources(fnames: List[String]): List[SourceFile] = {
    fnames.map(readSource(_))
  }

  type P <: Parser

  def newLexer(is: ANTLRInputStream): Lexer
  def newParser(tokens: CommonTokenStream): P
  def parserStart(parser: P): ParseTree

  def readSource(fname: String): SourceFile = {
    try {
      val is = new FileInputStream(fname)
      val input = new ANTLRInputStream(is)
      val lexer = newLexer(input)
      val tokens = new CommonTokenStream(lexer)
      val lines  = io.Source.fromFile(fname).getLines.toArray
      val parser = newParser(tokens)
      val tree = parserStart(parser)
      SourceFile(fname, lines, tree)
    } catch {
      case ioex: IOException =>
        // TODO: Fix this catch
        throw new IOException(ioex)
    }
  }
}
