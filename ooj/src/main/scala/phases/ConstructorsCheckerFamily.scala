package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.ooj.OojNodes
import sana.ooj.typechecker._



trait ConstructorsCheckerFamilyApi
  extends CheckerFamily[(Tree, ConstructorCheckerEnv)] {
  self =>

  override def default: Unit = ()

  def components: List[PartialFunction[(Tree, ConstructorCheckerEnv), Unit]] =
    generateComponents[(Tree, ConstructorCheckerEnv), Unit](
      "Program,CompilationUnit,PackageDef,ClassDef,Template,MethodDef,Block,Assign",
      "ConstructorsCheckerComponent", "check", "")

  def check: ((Tree, ConstructorCheckerEnv)) => Unit = family
}

case class ConstructorsCheckerFamily(compiler: CompilerInterface)
  extends ConstructorsCheckerFamilyApi



