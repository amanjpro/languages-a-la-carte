package ch.usi.inf.l3.sana.ooj

import ch.usi.inf.l3.sana
import sana.core.Implicits._
import sana.tiny
import sana.primj
import sana.ooj
import tiny.settings.SanaConfig
import tiny.ast.Tree
import tiny.source.SourceReader
import tiny.errors.ErrorReporting
import ooj.phases._
import ooj.antlr._

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._

trait Compiler extends tiny.CompilerApi[Tree, Unit] {
  self =>


  def langName: String = ooj.langName
  def langVersion: String = ooj.langVersion
  type ConfigType = SanaConfig
  def config: ConfigType
  def parser: tiny.parsers.Parser = ooj.parser.Parser

  ErrorReporting.isTest = config.isTest

  def sourceReader: SourceReader =new SourceReader {
    type P = Java1Parser
    def newLexer(is: ANTLRInputStream): Lexer =
      new Java1Lexer(is)
    def newParser(tokens: CommonTokenStream): Java1Parser =
      new Java1Parser(tokens)
    def parserStart(parser: Java1Parser): ParseTree = parser.compilationUnit
  }

  def compile: Tree => Unit = {
    Language.compile
  }

  object Language extends super.Language {
    def compile: Tree => Unit =
      (x: Tree) => {
        val f = (SymbolAssignerFamily.assign join
                  (NamerFamily.name join
                    (TyperFamily.typed join
                      ShapeCheckerFamily.check)))
        f((x, None))
      }
  }

  def start: Unit = {
    compile(parse(config.files.toList.head))
  }

}

class CompilerImpl(val config: SanaConfig) extends Compiler

