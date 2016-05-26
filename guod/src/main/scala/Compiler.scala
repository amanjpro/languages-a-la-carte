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
