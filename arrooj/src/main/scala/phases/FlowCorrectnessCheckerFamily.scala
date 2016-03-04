package ch.usi.inf.l3.sana.arrooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.arrooj.Nodes
import sana.ooj.typechecker._
import sana.arrooj.typechecker._



trait FlowCorrectnessCheckerFamily extends
  TransformationFamily[(Tree, FlowEnv), CompletenessStatus] {
  self =>

  override def default: CompletenessStatus = N

  def components:
    List[PartialFunction[(Tree, FlowEnv), CompletenessStatus]] =
    generateComponents[(Tree, FlowEnv), CompletenessStatus](
      Nodes.nodes,
      "FlowCorrectnessCheckerComponent", "check", "")

  def check: ((Tree, FlowEnv)) => CompletenessStatus = family
}

object FlowCorrectnessCheckerFamily extends
  FlowCorrectnessCheckerFamily



