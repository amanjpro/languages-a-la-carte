package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.symbols.Symbol
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.ooj.typechecker._



trait ForwardRefCheckerFamily extends CheckerFamily[(Tree, List[Symbol])] {
  self =>

  override def default: Unit = ()

  def components: List[PartialFunction[(Tree, List[Symbol]), Unit]] =
    generateComponents[(Tree, List[Symbol]), Unit](
      "Program,CompilationUnit,PackageDef,ClassDef,Template,Block",
      "ForwardRefCheckerComponent", "check", "")

  def check: ((Tree, List[Symbol])) => Unit = family
}

object ForwardRefCheckerFamily extends ForwardRefCheckerFamily



