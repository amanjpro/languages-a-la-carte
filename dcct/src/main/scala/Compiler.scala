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

package ch.usi.inf.l3.sana.dcct

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.dcct
import sana.ooj
import tiny.settings.SanaConfig
import tiny.core.Implicits._
import tiny.core.CompilerInterface
import tiny.ast.Implicits._
import tiny.ast.Tree
import tiny.types.Type
import tiny.names.Name
import tiny.symbols.Symbol
import tiny.modifiers.Flags
import sana.primj.modifiers._
import tiny.source.SourceReader
import tiny.errors.ErrorReporting
import tiny.debug.logger
import primj.symbols.{ProgramSymbol, MethodSymbol, VoidSymbol, VariableSymbol}
import primj.types.{ MethodType, VoidType }
import ooj.symbols.{PackageSymbol, ClassSymbol}
import ooj.modifiers._
import ooj.modifiers.Ops.noflags
import ooj.names.StdNames
import ooj.types.TypeUtils
import ooj.eval.Env
import dcct.phases._
import dcct.antlr._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import ch.usi.inf.l3.sana.dcct.phases.DcctCodeGenFamily
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
      dcct.symbols.SymbolUtils.standardDefinitions.foreach { x =>
        ProgramSymbol.declare(x)
      }
//      val tpe     = obj.tpe



//      val str = createClassSymbol(StdNames.STRING_TYPE_NAME,
//        None, TypeUtils.stringClassType)
// TODO I need to create new symbol utils and symbols for cloud types. 
      
//      ooj.symbols.SymbolUtils.standardDefinitions.foreach { s =>
//        ProgramSymbol.declare(s)
//      }
//      
      
    }

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

    private[this] lazy val symassigner   = DcctSymbolAssignerFamily(compiler)
    private[this] lazy val namer         = DcctNamerFamily(compiler)
    private[this] lazy val typer         = DcctTyperFamily(compiler)
    private[this] lazy val codegenerator         = DcctCodeGenFamily(compiler)

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

        val f = symassigner.assign join
                  namer.name  join
                    typer.typed join
                      codegenerator.codegen

        val targetCode = f(x)
        val targetFile = config.files.toList.head
        logger.info(s"[TARGET CODE]\n $targetCode ")
        logger.info(s"Writing target code to $targetFile")
        
      }
    }
  }
  def start: Unit = {
    compile(parse(config.files.toList.head))
  }

}

class CompilerImpl(val config: SanaConfig) extends Compiler
