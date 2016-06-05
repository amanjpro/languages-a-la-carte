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

package ch.usi.inf.l3.sana.primj

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.settings.SanaConfig
import tiny.core.CompilerInterface
import tiny.core.Implicits._
import tiny.ast.Tree
import tiny.ast.Implicits._
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

    // type-checking phases
    private[this] lazy val symassigner = PrimjSymbolAssignerFamily(compiler)
    private[this] lazy val namer       = PrimjNamerFamily(compiler)
    private[this] lazy val typer       = PrimjTyperFamily(compiler)

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
      (x: Tree) => {
        val f =
          (symassigner.assign join
            (namer.name join
              (typer.typed join
                (PrimjShapeCheckerFamily(compiler).check))))
        f(x)
      }
    }
  }
  def start: Unit = {
    compile(parse(config.files.toList.head))
  }

}

class CompilerImpl(val config: SanaConfig) extends Compiler
