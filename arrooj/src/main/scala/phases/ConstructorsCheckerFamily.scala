package ch.usi.inf.l3.sana.arrooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.arrooj.Nodes
import sana.ooj.typechecker._



trait ConstructorsCheckerFamily
  extends CheckerFamily[(Tree, ConstructorCheckerEnv)] {
  self =>

  override def default: Unit = ()

  def components: List[PartialFunction[(Tree, ConstructorCheckerEnv), Unit]] =
    generateComponents[(Tree, ConstructorCheckerEnv), Unit](
      "Program,CompilationUnit,PackageDef,ClassDef,Template,MethodDef,Block,Assign",
      "ConstructorsCheckerComponent", "check", "")

  def check: ((Tree, ConstructorCheckerEnv)) => Unit = family
}

object ConstructorsCheckerFamily extends ConstructorsCheckerFamily


