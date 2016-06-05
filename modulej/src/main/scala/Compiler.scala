/*
 * Copyright (c) <2015-2016>, see CONTRIBUTERS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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

  protected val language = new Language

  def sourceReader: SourceReader =new SourceReader {
    type P = Java1Parser
    def newLexer(is: ANTLRInputStream): Lexer =
      new Java1Lexer(is)
    def newParser(tokens: CommonTokenStream): Java1Parser =
      new Java1Parser(tokens)
    def parserStart(parser: Java1Parser): ParseTree = parser.compilationUnit
  }

  def compile: Tree => Unit = {
    language.compile
  }

  class Language extends super.Language {
    self =>
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
    protected lazy val symassigner = SymbolAssignerFamily(compiler)
    protected lazy val namer       = NamerFamily(compiler)
    protected lazy val deftyper    = DefTyperFamily(compiler)
    protected lazy val typer       = TyperFamily(compiler)

    protected val classpath = config.classpath.toList.map(new java.io.File(_))
    protected val loader    = new ClassPathLoader(classpath)
    val compiler: CompilerInterface = new CompilerInterface {
      val classpath = self.classpath
      val loader    = self.loader

      def typeCheck(owner: Option[Symbol])(tree: Tree): Tree = {
        owner.foreach(tree.owner = _)
        symassigner.assign.join(
          namer.name.join(deftyper.typed.join(typer.typed)))(tree)
      }

      override def resolveNames(owner: Option[Symbol])(tree: Tree): Tree = {
        owner.foreach(tree.owner = _)
        symassigner.assign.join(namer.name)(tree)
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


    protected val constantFolder = (t: Tree) => {
      val env = ConstantCollectingFamily(compiler)
        .collect((t, Env.emptyEnv))
      val (tf, _)   = ConstantFoldingFamily(compiler)
        .constantFold((t, env))
      tf
    }
    protected val labelChecker = (t: Tree) =>
      LabelNameCheckerFamily(compiler).check((t, Nil))
    protected val jumpChecker = (t: Tree) =>
      JumpCheckerFamily(compiler).check((t, Nil))
    protected val forwardRefChecker = (t: Tree) =>
      ForwardRefCheckerFamily(compiler).check((t, Nil))
    protected val constructorsChecker = (t: Tree) =>
      ConstructorsCheckerFamily(compiler)
        .check((t, new ConstructorCheckerEnv))
    protected val flowAnalyzer = (t: Tree) => {
      FlowCorrectnessCheckerFamily(compiler).check((t, new FlowEnv))
      t
    }
    protected val exceptionHandlingChecker = (t: Tree) => {
      ExceptionHandlingCheckerFamily(compiler).check((t, Nil))
      t
    }

    def compile: Tree => Unit = {
      (x: Tree) => {

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
    language.init()
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
