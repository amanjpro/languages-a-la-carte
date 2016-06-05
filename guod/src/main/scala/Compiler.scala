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

package ch.usi.inf.l3.sana.guod

import ch.usi.inf.l3.sana
import sana.modulej
import sana.guod
import sana.tiny
import guod.ast._
import guod.phases._
import modulej.phases._
import tiny.errors.ErrorReporting
import tiny.core.Implicits._
import tiny.settings.SanaConfig
import guod.codegen.{Env, ByteCodeWriter}

trait Compiler extends modulej.Compiler {
  self =>


  override protected val language = new Language
  class Language extends super.Language {


    protected val qualifiers     = QualifierFamily(compiler).fullyqualify
    protected val localvariables =
      (t: Tree) => LocalVariablesFamily(compiler).subst((t, new Env))
    protected val initializers   = InitializersFamily(compiler).inline
    protected val generate       =
      (t: Tree) => CodeGenFamily(compiler).codegen((t,
        ByteCodeWriter("")))
    def codegen: Tree => Unit = (t: Tree) => {
      t match {
        case p               if !ErrorReporting.isErroneous             =>
          val f = initializers join
                    qualifiers join
                      localvariables join
                        generate
          f(p)
        case _                                                          =>
          ()
      }
    }


    override def compile: Tree => Unit = {
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
                                      exceptionHandlingChecker join
                                        codegen
        f(x)
      }
    }
  }
}

class CompilerImpl(val config: SanaConfig) extends Compiler
