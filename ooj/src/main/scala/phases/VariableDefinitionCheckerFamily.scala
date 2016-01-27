package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.ooj.OojNodes
import sana.ooj.typechecker._



trait VariableDefinitionCheckerFamily extends
  TransformationFamily[(Tree, DefinitiveAssignedEnv), Boolean] {
  self =>

  override def default: Boolean = false

  def components:
    List[PartialFunction[(Tree, DefinitiveAssignedEnv), Boolean]] =
    generateComponents[(Tree, DefinitiveAssignedEnv), Boolean](OojNodes.nodes,
      "VariableDefinitionCheckerComponent", "check", "")

  def check: ((Tree, DefinitiveAssignedEnv)) => Boolean = family
}

object VariableDefinitionCheckerFamily extends
  VariableDefinitionCheckerFamily



