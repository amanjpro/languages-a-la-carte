package ch.usi.inf.l3.sana.modulej

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.ooj
import sana.arrayj
import sana.primj
import sana.modulej
import sana.robustj
import tiny.core.CompilerInterface
import tiny.core.Implicits._
import tiny.settings.SanaConfig
import tiny.ast.Tree
import modulej.ast.Implicits._
import tiny.types.Type
import tiny.symbols.Symbol
import tiny.modifiers.Flags
import tiny.source.SourceReader
import tiny.errors.ErrorReporting
import tiny.names.Name
import calcj.types._
import calcj.symbols._
import primj.symbols.ProgramSymbol
import modulej.symbols.SymbolUtils
import modulej.phases._
import modulej.ast.TreeFactories
import modulej.classpath.ClassPathLoader
import robustj.types.TypeUtils
import ooj.modifiers._
import primj.modifiers._
import modulej.parser.Parser
import ooj.antlr.{Java1Parser, Java1Lexer}
import ooj.symbols.{PackageSymbol, ClassSymbol}
import ooj.modifiers.Ops.noflags
import robustj.names.StdNames
import ooj.eval.Env
import ooj.typechecker.{ConstructorCheckerEnv, FlowEnv}

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._

trait Compiler extends tiny.CompilerApi[Tree, Unit] {
  self =>


  def langName: String = modulej.langName
  def langVersion: String = modulej.langVersion
  type ConfigType = SanaConfig
  def config: ConfigType
  def parser: tiny.parsers.Parser = modulej.parser.Parser

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
    def init(): Unit = {


      SymbolUtils.standardDefinitions.foreach { s =>
        ProgramSymbol.declare(s)
      }

      val javaPackageSymbol: PackageSymbol = {
        val name    = StdNames.JAVA_PACKAGE_NAME
        val owner = Some(ProgramSymbol)
        PackageSymbol(name, owner)
      }
      ProgramSymbol.declare(javaPackageSymbol)

      val langPackageSymbol: PackageSymbol = {
        val name    = StdNames.LANG_PACKAGE_NAME
        val owner = Some(javaPackageSymbol)
        PackageSymbol(name, owner)
      }
      javaPackageSymbol.declare(langPackageSymbol)
    }

    // type-checking phases
    private[this] lazy val symassigner = SymbolAssignerFamily(compiler)
    private[this] lazy val namer       = NamerFamily(compiler)
    private[this] lazy val deftyper    = DefTyperFamily(compiler)
    private[this] lazy val typer       = TyperFamily(compiler)

    def compiler: CompilerInterface = new CompilerInterface {
      val classpath =
        config.classpath.toList.map(new java.io.File(_))
      val loader    = new ClassPathLoader(classpath)

      def typeCheck(owner: Option[Symbol])(tree: Tree): Tree = {
        owner.foreach(tree.owner = _)
        symassigner.assign.join(
          namer.name.join(deftyper.typed.join(typer.typed)))(tree)
      }
      def definesModule(module: String): Boolean = {
        loader.defines(module, false)
      }

      def parse(source: String): Tree   = ???

      def load(fname: String): Option[Tree]   = {
        val otree = loader.load(fname)
        otree.map { tree =>
          symassigner.assign.join(namer.name.join(deftyper.typed))(tree)
        }
      }

      def unparse(tree: Tree): String   = ???
    }

    def compile: Tree => Unit = {
      (x: Tree) => {
        val constantFolder = (t: Tree) => {
          val (tc, env) = ConstantCollectingFamily(compiler)
            .collect((t, Env.emptyEnv))
          val (tf, _)   = ConstantFoldingFamily(compiler)
            .constantFold((tc, env))
          tf
        }
        val labelChecker = (t: Tree) =>
          LabelNameCheckerFamily(compiler).check((t, Nil))
        val jumpChecker = (t: Tree) =>
          JumpCheckerFamily(compiler).check((t, Nil))
        val forwardRefChecker = (t: Tree) =>
          ForwardRefCheckerFamily(compiler).check((t, Nil))
        val constructorsChecker = (t: Tree) =>
          ConstructorsCheckerFamily(compiler)
            .check((t, new ConstructorCheckerEnv))
        val flowAnalyzer = (t: Tree) => {
          FlowCorrectnessCheckerFamily(compiler).check((t, new FlowEnv))
          t
        }
        val exceptionHandlingChecker = (t: Tree) => {
          ExceptionHandlingCheckerFamily(compiler).check((t, Nil))
          t
        }

        val f = symassigner.assign join
                  namer.name join
                    deftyper.typed join
                      constantFolder join
                        typer.typed join
                          ShapeCheckerFamily(compiler).check join
                            labelChecker join
                              jumpChecker join
                                forwardRefChecker join
                                  constructorsChecker join
                                    flowAnalyzer join
                                      exceptionHandlingChecker
        f(x)
      }
    }
  }

  def start: Unit = {
    Language.init()
    val cunits  = {
      val files = config.files.filter(_.endsWith(".java"))
      files.map(f => parse(f))
    }
    if(cunits != Nil) {
      val program = TreeFactories.mkProgram(cunits)
      compile(program)
    } else
      println("No files to compile")
  }

}

class CompilerImpl(val config: SanaConfig) extends Compiler

