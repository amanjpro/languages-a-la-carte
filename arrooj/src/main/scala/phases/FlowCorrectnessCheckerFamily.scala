package ch.usi.inf.l3.sana.arrooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.arrooj.Nodes
import sana.ooj.typechecker._
import sana.arrooj.typechecker._



trait FlowCorrectnessCheckerFamilyApi extends
  TransformationFamily[(Tree, FlowEnv), CompletenessStatus] {
  self =>

  override def default = { case s => N }

  def components:
    List[PartialFunction[(Tree, FlowEnv), CompletenessStatus]] =
    generateComponents[(Tree, FlowEnv), CompletenessStatus](
      Nodes.nodes,
      "FlowCorrectnessCheckerComponent", "check", "")

  def check: ((Tree, FlowEnv)) => CompletenessStatus = family
}

case class FlowCorrectnessCheckerFamily(compiler: CompilerInterface) extends
  FlowCorrectnessCheckerFamilyApi



