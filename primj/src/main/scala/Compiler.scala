package ch.usi.inf.l3.sana.primj

import ch.usi.inf.l3.sana
import sana.core.Implicits._
import sana.tiny
import sana.primj
import tiny.settings.SanaConfig
import tiny.ast.Tree
import tiny.source.SourceReader
import tiny.errors.ErrorReporting
import tiny.symbols.Symbol
import primj.symbols.{ProgramSymbol, SymbolUtils}
import primj.phases._
import primj.antlr._

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
trait Compiler extends tiny.CompilerApi[Tree, Unit] {
  self =>


  def langName: String = "Primj"
  def langVersion: String = "1.0.0"
  type ConfigType = SanaConfig
  def config: ConfigType
  def parser: tiny.parsers.Parser = primj.parsers.Parser

  ErrorReporting.isTest = config.isTest

  def sourceReader: SourceReader = new SourceReader {
    type P = PrimjParser
    def newLexer(is: ANTLRInputStream): Lexer =
      new PrimjLexer(is)
    def newParser(tokens: CommonTokenStream): PrimjParser =
      new PrimjParser(tokens)
    def parserStart(parser: PrimjParser): ParseTree = parser.program
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
        val typers =
          (t: Tree) => PrimjTyperFamily.typed((t, Nil))

        val f =
          (PrimjSymbolAssignerFamily.assign join
            (PrimjNamerFamily.name join
              (typers join
                (PrimjShapeCheckerFamily.check))))
        f(x)
      }
    }
  }
  def start: Unit = {
    compile(parse(config.files.toList.head))
  }

}

class CompilerImpl(val config: SanaConfig) extends Compiler

