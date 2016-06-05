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

package ch.usi.inf.l3.sana.ooj

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.ooj
import sana.primj
import tiny.core.CompilerInterface
import tiny.core.Implicits._
import tiny.settings.SanaConfig
import tiny.ast.Tree
import tiny.ast.Implicits._
import tiny.types.Type
import tiny.symbols.Symbol
import tiny.modifiers.Flags
import tiny.source.SourceReader
import tiny.errors.ErrorReporting
import tiny.names.Name
import calcj.types._
import calcj.symbols._
import primj.symbols.{ProgramSymbol, MethodSymbol, VariableSymbol, VoidSymbol}
import primj.types._
import primj.modifiers.{PARAM, FINAL}
import ooj.ast.TreeFactories
import ooj.phases._
import ooj.symbols.{PackageSymbol, SymbolUtils, ClassSymbol}
import ooj.modifiers._
import ooj.modifiers.Ops.noflags
import ooj.names.StdNames
import ooj.types.TypeUtils
import ooj.eval.Env
import ooj.typechecker.ConstructorCheckerEnv
import ooj.typechecker.FlowEnv
import ooj.antlr._

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._

trait Compiler extends tiny.CompilerApi[Tree, Unit] {
  self =>


  def langName: String = ooj.langName
  def langVersion: String = ooj.langVersion
  type ConfigType = SanaConfig
  def config: ConfigType
  def parser: tiny.parsers.Parser = ooj.parser.Parser

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
      def singleParamConstructor(paramSymbol: Symbol,
          owner: Symbol): MethodSymbol = {
        val mods = PUBLIC_ACC | CONSTRUCTOR
        val name = StdNames.CONSTRUCTOR_NAME
        val cnstrTpe = Some(MethodType(VoidType, paramSymbol.tpe.toList))
        val sym  = MethodSymbol(mods, name,
            Some(VoidSymbol), Nil, cnstrTpe, Some(owner))
        val psym    = VariableSymbol(PARAM | noflags,
            Name("value"), Some(paramSymbol), Some(sym))
        sym.params = List(psym)
        sym
      }

      def createClassSymbol(name: Name, paramSym: Option[Symbol],
              tpe: Type): ClassSymbol = {
        val mods    = PUBLIC_ACC | FINAL
        // val name    = StdNames.STRING_TYPE_NAME
        val parents = List(SymbolUtils.objectClassSymbol)
        val owner   = Some(SymbolUtils.langPackageSymbol)
        // val tpe     = Some(TypeUtils.stringClassType)
        val res     = ClassSymbol(mods, name, parents, owner, Some(tpe))
        val cnstr   = paramSym match {
          case None            =>
            singleParamConstructor(res, res)
          case Some(sym)       =>
            singleParamConstructor(sym, res)
        }
        res.declare(cnstr)
        res
      }

      val javaPackageSymbol: PackageSymbol = {
        val name    = StdNames.JAVA_PACKAGE_NAME
        val owner = Some(ProgramSymbol)
        PackageSymbol(name, owner)
      }

      val langPackageSymbol: PackageSymbol = {
        val name    = StdNames.LANG_PACKAGE_NAME
        val owner = Some(javaPackageSymbol)
        PackageSymbol(name, owner)
      }

      val obj: ClassSymbol = {
        val mods    = Flags(PUBLIC_ACC)
        val name    = StdNames.OBJECT_TYPE_NAME
        val parents = Nil
        val owner   = Some(langPackageSymbol)
        val tpe     = Some(TypeUtils.objectClassType)
        val res = ClassSymbol(mods, name, parents, owner, tpe)
        res
      }
      langPackageSymbol.declare(obj)
      javaPackageSymbol.declare(langPackageSymbol)
      ProgramSymbol.declare(javaPackageSymbol)

      val tpe     = obj.tpe
      // cnstr tpe:
      val cnstrTpe = Some(MethodType(VoidType, Nil))
      val cnstr = MethodSymbol(PUBLIC_ACC | CONSTRUCTOR,
        StdNames.CONSTRUCTOR_NAME, Some(VoidSymbol), Nil,
        cnstrTpe, Some(obj))

      // eqls tpe:
      val eqlsTpe = Some(MethodType(BooleanType, tpe.toList))
      val eqls = MethodSymbol(PUBLIC_ACC | noflags,
        Name("equals"), Some(BooleanSymbol), Nil, eqlsTpe, Some(obj))
      val psym    = VariableSymbol(PARAM | noflags,
            Name("other"), Some(obj), Some(eqls))

      eqls.params = List(psym)



      val str = createClassSymbol(StdNames.STRING_TYPE_NAME,
        None, TypeUtils.stringClassType)

      val bool = createClassSymbol(StdNames.BOOLEAN_CLASS_NAME,
        Some(BooleanSymbol), TypeUtils.booleanClassType)

      val char = createClassSymbol(StdNames.CHARACTER_CLASS_NAME,
        Some(CharSymbol), TypeUtils.characterClassType)

      val int = createClassSymbol(StdNames.INTEGER_CLASS_NAME,
        Some(IntSymbol), TypeUtils.integerClassType)

      val long = createClassSymbol(StdNames.LONG_CLASS_NAME,
        Some(LongSymbol), TypeUtils.longClassType)

      val float = createClassSymbol(StdNames.FLOAT_CLASS_NAME,
        Some(FloatSymbol), TypeUtils.floatClassType)

      val double = createClassSymbol(StdNames.DOUBLE_CLASS_NAME,
        Some(DoubleSymbol), TypeUtils.doubleClassType)

      val toStrTpe = Some(MethodType(TypeUtils.stringClassType, Nil))
      val toStr = MethodSymbol(Flags(PUBLIC_ACC),
        Name("toString"), Some(str), Nil,
        toStrTpe, Some(obj))


      obj.declare(cnstr)
      obj.declare(eqls)
      obj.declare(toStr)
      SymbolUtils.langPackageSymbol.declare(str)
      SymbolUtils.langPackageSymbol.declare(bool)
      SymbolUtils.langPackageSymbol.declare(char)
      SymbolUtils.langPackageSymbol.declare(int)
      SymbolUtils.langPackageSymbol.declare(long)
      SymbolUtils.langPackageSymbol.declare(float)
      SymbolUtils.langPackageSymbol.declare(double)


      SymbolUtils.standardDefinitions.foreach { s =>
        ProgramSymbol.declare(s)
      }
    }

    // type-checking phases
    private[this] lazy val symassigner = SymbolAssignerFamily(compiler)
    private[this] lazy val namer       = NamerFamily(compiler)
    private[this] lazy val deftyper    = DefTyperFamily(compiler)
    private[this] lazy val typer       = TyperFamily(compiler)

    def compiler: CompilerInterface = new CompilerInterface {
      def typeCheck(owner: Option[Symbol])(tree: Tree): Tree = {
        owner.foreach(tree.owner = _)
        symassigner.assign.join(
          namer.name.join(deftyper.typed.join(typer.typed)))(tree)
      }
      def definesModule(module: String): Boolean = false
      def parse(source: String): Tree   = ???
      def load(fname: String): Option[Tree]   = None
      def unparse(tree: Tree): String   = ???
    }



    def compile: Tree => Unit = {
      (x: Tree) => {
        val constantFolder = (t: Tree) => {
          val env = ConstantCollectingFamily(compiler).collect(
            (t, Env.emptyEnv))
          val (tf, _)   = ConstantFoldingFamily(compiler).constantFold((t, env))
          tf
        }
        val labelChecker = (t: Tree) =>
          LabelNameCheckerFamily(compiler).check((t, Nil))
        val jumpChecker = (t: Tree) =>
          JumpCheckerFamily(compiler).check((t, Nil))
        val forwardRefChecker = (t: Tree) =>
          ForwardRefCheckerFamily(compiler).check((t, Nil))
        val constructorsChecker = (t: Tree) =>
          ConstructorsCheckerFamily(compiler).check(
            (t, new ConstructorCheckerEnv))
        val flowAnalyzer = (t: Tree) => {
          FlowCorrectnessCheckerFamily(compiler).check((t, new FlowEnv))
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
                                    flowAnalyzer
        f(x)
      }
    }
  }

  def start: Unit = {
    Language.init()
    val cunits  = config.files.map(f => parse(f)).toList
    val program = TreeFactories.mkProgram(cunits)
    compile(program)
  }

}

class CompilerImpl(val config: SanaConfig) extends Compiler
