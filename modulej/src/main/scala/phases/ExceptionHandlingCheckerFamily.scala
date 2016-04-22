package ch.usi.inf.l3.sana.modulej.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.modulej.Nodes
import sana.ooj.typechecker._
import sana.arrooj.typechecker._
import sana.robustj.typechecker._
import sana.ppj.typechecker._
import sana.dynj.typechecker._



trait ExceptionHandlingCheckerFamilyApi extends
  TransformationFamily[(Tree, List[HandledException]),
                                        List[HandledException]] {
  self =>

  override def default: List[HandledException] = Nil

  def components:
    List[PartialFunction[(Tree, List[HandledException]),
              List[HandledException]]] =
    generateComponents[(Tree, List[HandledException]), List[HandledException]](
      Nodes.nodes,
      "ExceptionHandlingCheckerComponent", "check", "Import")

  def check: ((Tree, List[HandledException])) => List[HandledException] =
    family
}

case class ExceptionHandlingCheckerFamily(compiler: CompilerInterface)
  extends ExceptionHandlingCheckerFamilyApi



