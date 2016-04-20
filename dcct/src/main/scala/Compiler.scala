package ch.usi.inf.l3.sana.dcct

import ch.usi.inf.l3.sana
import sana.core.Implicits._
import sana.tiny
import sana.primj
import sana.dcct
import sana.ooj
import tiny.settings.SanaConfig
import tiny.ast.Tree
import tiny.types.Type
import tiny.names.Name
import tiny.symbols.Symbol
import tiny.modifiers.Flags
import sana.primj.modifiers._
import tiny.source.SourceReader
import tiny.errors.ErrorReporting
import primj.symbols.{ProgramSymbol, SymbolUtils, MethodSymbol, VoidSymbol, VariableSymbol}
import primj.types.{ MethodType, VoidType }
import ooj.symbols.{PackageSymbol, SymbolUtils, ClassSymbol}
import ooj.modifiers._
import ooj.modifiers.Ops.noflags
import ooj.names.StdNames
import ooj.types.TypeUtils
import ooj.eval.Env

import dcct.phases._
import dcct.antlr._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
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
        val parents = List(ooj.symbols.SymbolUtils.objectClassSymbol)
        val owner   = Some(ooj.symbols.SymbolUtils.langPackageSymbol)
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



//      val str = createClassSymbol(StdNames.STRING_TYPE_NAME,
//        None, TypeUtils.stringClassType)
// TODO I need to create new symbol utils and symbols for cloud types. 
      
      ooj.symbols.SymbolUtils.standardDefinitions.foreach { s =>
        ProgramSymbol.declare(s)
      }
    }
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

        val f =
         ( ( (DcctSymbolAssignerFamily.assign) join
            (DcctNamerFamily.name) ) join (DcctCodeGenFamily.codegen) )

        f(x)
      }
    }
  }
  def start: Unit = {
    compile(parse(config.files.toList.head))
  }

}

class CompilerImpl(val config: SanaConfig) extends Compiler

