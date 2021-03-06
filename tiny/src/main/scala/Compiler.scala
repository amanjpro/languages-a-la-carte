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

  /** The name of this language */
  def langName: String

  /** The version of this language */
  def langVersion: String

  type ConfigType <: SanaConfig

  /** The configurations of this compilation run */
  def config: ConfigType

  def parser: Parser

  ErrorReporting.isTest = config.isTest

  def sourceReader: SourceReader

  def compile: Input => Output


  trait Language extends LanguageModule[Input, Output] {
    /** Initializes the standard definitions and the compiler */
    def init(): Unit

    /** Generates target-codes */
    def codegen(tree: Tree): Unit = ()

    /** Returns the compiler interface */
    def compiler: CompilerInterface

    /** Compiles the input */
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
