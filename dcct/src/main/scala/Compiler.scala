package ch.usi.inf.l3.sana.dcct

import ch.usi.inf.l3.sana
import sana.core.Implicits._
import sana.tiny
import sana.primj
import sana.dcct
import tiny.settings.SanaConfig
import tiny.ast.Tree
import tiny.source.SourceReader
import tiny.errors.ErrorReporting
import tiny.symbols.Symbol
import primj.symbols.{ProgramSymbol, SymbolUtils}
import primj.phases._
import primj.antlr._
import dcct.phases._
import dcct.antlr._

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
trait Compiler extends tiny.CompilerApi[Tree, Unit] {
  self =>


  def langName: String = "DCCT"
  def langVersion: String = "1.0.0"
  type ConfigType = SanaConfig
  def config: ConfigType

  //TODO Not sure what is happening here... Was  primj.parsers.Parser
  def parser: tiny.parsers.Parser = dcct.parsers.Parser

  ErrorReporting.isTest = config.isTest

  def sourceReader: SourceReader = new SourceReader {
    type P = DcctParser
    def newLexer(is: ANTLRInputStream): Lexer =
      new DcctLexer(is)
    def newParser(tokens: CommonTokenStream): DcctParser =
      new DcctParser(tokens)
    def parserStart(parser: DcctParser): ParseTree = parser.program
  }

  def compile: Tree => Unit = {
    Language.compile
  }

  object Language extends super.Language {
    def init(): Unit = {
      SymbolUtils.standardDefinitions.foreach { s =>
        ProgramSymbol.declare(s)
      }
    }
    def compile: Tree => Unit = {
      init()
      (x: Tree) => {

        // Here we add and join families, families are like phases. This was
        // like the following before: 
        // val f =
        //  (PrimjSymbolAssignerFamily.assign join
        //    (PrimjNamerFamily.name join
        //      (PrimjTyperFamily.typed join
        //        (PrimjShapeCheckerFamily.check))))

        val f =
          (DcctSymbolAssignerFamily.assign)

        f(x)
      }
    }
  }
  def start: Unit = {
    compile(parse(config.files.toList.head))
  }

}

class CompilerImpl(val config: SanaConfig) extends Compiler

