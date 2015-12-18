package ch.usi.inf.l3.sana.ooj

import ch.usi.inf.l3.sana
import sana.core.Implicits._
import sana.tiny
import sana.calcj
import sana.ooj
import sana.primj
import tiny.settings.SanaConfig
import tiny.ast.Tree
import tiny.modifiers.Flags
import tiny.source.SourceReader
import tiny.errors.ErrorReporting
import tiny.names.Name
import calcj.types._
import calcj.symbols.BooleanSymbol
import primj.symbols.{MethodSymbol, VariableSymbol}
import primj.types._
import primj.modifiers.{PARAM, FINAL}
import ooj.phases._
import ooj.symbols.{ProgramSymbol, SymbolUtils, ClassSymbol}
import ooj.modifiers._
import ooj.modifiers.Ops.noflags
import ooj.names.StdNames
import ooj.types.TypeUtils
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
      val obj = SymbolUtils.objectClassSymbol
      val tpe     = obj.tpe
      // cnstr tpe:
      val cnstrTpe = Some(MethodType(VoidType, Nil))
      val cnstr = MethodSymbol(PUBLIC_ACC | CONSTRUCTOR,
        StdNames.CONSTRUCTOR_NAME, Nil, Some(obj),
        cnstrTpe, Some(obj))

      // eqls tpe:
      val eqlsTpe = Some(MethodType(BooleanType, tpe.toList))
      val eqls = MethodSymbol(PUBLIC_ACC | noflags,
        Name("equals"), Nil, Some(BooleanSymbol), eqlsTpe, Some(obj))
      val psym    = VariableSymbol(PARAM | noflags,
            Name("other"), Some(obj), Some(eqls))

      eqls.params = List(psym)



      val str = {
        val mods    = PUBLIC_ACC | FINAL
        val name    = StdNames.STRING_TYPE_NAME
        val parents = List(obj)
        val owner   = Some(SymbolUtils.langPackageSymbol)
        val tpe     = Some(TypeUtils.stringClassType)
        ClassSymbol(mods, name, parents, owner, tpe)
      }

      val toStrTpe = Some(MethodType(TypeUtils.stringClassType, Nil))
      val toStr = MethodSymbol(Flags(PUBLIC_ACC),
        Name("toString"), Nil, Some(str),
        toStrTpe, Some(obj))


      obj.declare(cnstr)
      obj.declare(eqls)
      obj.declare(toStr)
      SymbolUtils.langPackageSymbol.declare(str)
    }

    def compile: Tree => Unit = {
      init()
      (x: Tree) => {
        val typers = (t: Tree) => TyperFamily.typed((t, Nil))
        val f = (SymbolAssignerFamily.assign join
                  (NamerFamily.name join
                    (TypeTyperFamily.typed join
                      (DefTyperFamily.typed join
                        (typers join
                          ShapeCheckerFamily.check)))))
        f((x, Some(ProgramSymbol)))
      }
    }
  }

  def start: Unit = {
    compile(parse(config.files.toList.head))
  }

}

class CompilerImpl(val config: SanaConfig) extends Compiler

