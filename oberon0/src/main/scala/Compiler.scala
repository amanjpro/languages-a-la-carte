/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
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

package ch.usi.inf.l3.sana.oberon0

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.oberon0
import sana.ooj
import sana.calcj
import tiny.settings.SanaConfig
import tiny.core.CompilerInterface
import tiny.core.Implicits._
import tiny.ast.Tree
import tiny.ast.Implicits._
import tiny.source.SourceReader
import tiny.errors.ErrorReporting
import tiny.symbols.Symbol
import tiny.names.Name
import ooj.modifiers._
import primj.modifiers._
import tiny.modifiers.Ops._
import primj.symbols.{ProgramSymbol, VoidSymbol, MethodSymbol, VariableSymbol}
import primj.types.{VoidType, MethodType}
import oberon0.symbols.IntSymbol
import calcj.types.IntType
import oberon0.symbols.SymbolUtils
import oberon0.phases._
import oberon0.ast.TreeFactories
import tiny.ast.DefTree
import oberon0.antlr._

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
trait Compiler extends tiny.CompilerApi[Tree, Unit] {
  self =>


  def langName: String = "Oberon-0"
  def langVersion: String = "1.0.0"
  type ConfigType = SanaConfig
  def config: ConfigType
  def parser: tiny.parsers.Parser = oberon0.parsers.Parser

  ErrorReporting.isTest = config.isTest

  def sourceReader: SourceReader = new SourceReader {
    type P = Oberon0Parser
    def newLexer(is: ANTLRInputStream): Lexer =
      new Oberon0Lexer(is)
    def newParser(tokens: CommonTokenStream): Oberon0Parser =
      new Oberon0Parser(tokens)
    def parserStart(parser: Oberon0Parser): ParseTree = parser.module
  }

  def compile: Tree => Unit = {
    Language.compile
  }


  object Language extends super.Language {
    def init(): Unit = {
      SymbolUtils.standardDefinitions.foreach { s =>
        ProgramSymbol.declare(s)
      }

      val read    = {
        val tpe    = Some(MethodType(VoidType, List(IntType)))
        val symbol = MethodSymbol(PUBLIC_ACC | STATIC,
          Name("Read"), Some(VoidSymbol), Nil, tpe, Some(ProgramSymbol))
        val param  = VariableSymbol(PARAM | noflags,
          Name("val"), Some(IntSymbol), Some(symbol))
        symbol.params = List(param)
        symbol
      }
      val write   = {
        val tpe    = Some(MethodType(VoidType, List(IntType)))
        val symbol = MethodSymbol(PUBLIC_ACC | STATIC,
          Name("Write"), Some(VoidSymbol), Nil, tpe, Some(ProgramSymbol))
        val param  = VariableSymbol(PARAM | noflags,
          Name("val"), Some(IntSymbol), Some(symbol))
        symbol.params = List(param)
        symbol
      }
      val writeLn = {
        val tpe    = Some(MethodType(VoidType, Nil))
        MethodSymbol(PUBLIC_ACC | STATIC,
          Name("WriteLn"), Some(VoidSymbol), Nil, tpe, Some(ProgramSymbol))
      }

      ProgramSymbol.declare(read)
      ProgramSymbol.declare(write)
      ProgramSymbol.declare(writeLn)
    }

    // type-checking phases
    private[this] lazy val symassigner = SymbolAssignerFamily(compiler)
    private[this] lazy val namer       = NamerFamily(compiler)
    private[this] lazy val typer       = TyperFamily(compiler)

    def compiler: CompilerInterface = new CompilerInterface {
      def typeCheck(owner: Option[Symbol])(tree: Tree): Tree = {
        owner.foreach(tree.owner = _)
        symassigner.assign.join(namer.name.join(typer.typed))(tree)
      }
      def definesModule(module: String): Boolean = false
      def load(fname: String): Option[Tree]   = None
      def parse(source: String): Tree   = ???
      def unparse(tree: Tree): String   = ???
    }

    def compile: Tree => Unit = {
      init()
      val codegen = (t: Tree) => {
        CodeGenFamily(compiler).codegen((t, 0))
      }
      (x: Tree) => {
        val f = (symassigner.assign join
                  (namer.name join
                    (typer.typed join
                      (codegen))))
        println(f(x))
      }
    }
  }
  def start: Unit = {
    val program = {
      val name = config.files.toList.head
      TreeFactories.mkProgram(List(parse(name).asInstanceOf[DefTree]), name)
    }
    compile(program)
  }

}

class CompilerImpl(val config: SanaConfig) extends Compiler
