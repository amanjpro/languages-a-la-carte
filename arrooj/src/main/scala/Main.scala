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

package ch.usi.inf.l3.sana.arrooj

import ch.usi.inf.l3.sana
import sana.tiny
import sana.arrooj
import tiny.settings.{SanaConfig,CommandLineArgumentParser}
import tiny.errors.ErrorReporting
import tiny.debug.logger

object Main {
  def processOptions(args: Array[String],
                        ln: String,
                        lv: String,
                        fn: String): Either[String, SanaConfig] = {
        val config = new SanaConfig
        val processor = new CommandLineArgumentParser(config, ln, lv, fn)
        if(processor.parser.parse(args)) {
          Right(config)
        } else {
          Left("Bad set of arguments\n" ++ processor.parser.usage)
        }
      }

  def main(args: Array[String]): Unit = {


    val config = processOptions(args, langName, langVersion,
        tiny.frameworkName) match {
      case Right(config) => config
      case Left(msg)     =>
        println(msg)
        System.exit(1)
        ???  // To satisfy the type checker
    }

    val ln = langName
    val lv = langVersion
    logger.setLevel(config.logLevel)

    val compiler = new CompilerImpl(config)
    compiler.start
    ErrorReporting.isErroneous match {
      case true     =>
        ErrorReporting.errors.foreach(Console.err.println(_))
      case _        =>
        println("Compilation Successful")
    }

    // TODO: Here it should go to codegen
    // units.foreach(println)
  }
}
