package ch.usi.inf.l3.sana.tiny


import ch.usi.inf.l3.sana.tiny.core.{LanguageModule, CompilerInterface}
import source.{SourceReader, SourceFile}
import parsers.Parser
import ast.Tree
import debug.logger
import util._
import settings.SanaConfig
import types.Type
import errors._


trait CompilerApi[Input, Output] {

  def langName: String
  def langVersion: String
  type ConfigType <: SanaConfig
  def config: ConfigType
  def parser: Parser

  ErrorReporting.isTest = config.isTest

  def sourceReader: SourceReader

  def compile: Input => Output


  trait Language extends LanguageModule[Input, Output] {
    def init(): Unit
    def codegen(tree: Tree): Unit = ()
    def compiler: CompilerInterface
    def compile: Input => Output
  }

  protected def parse(file: String): Tree = {
    val source: SourceFile = sourceReader.readSource(file)
    parser.parse(source)
  }


  // def attach(units: List[global.CompilationUnit]):
  //     (Vector[Report], List[global.CompilationUnit]) = {
  //   compile(units)
  // }

  def start: Output
    // compile(parse(config.files.toList))
  // }
}




