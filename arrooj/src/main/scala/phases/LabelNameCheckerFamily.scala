package ch.usi.inf.l3.sana.arrooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.brokenj.ast.{LabelApi}
import sana.arrooj.Nodes
import sana.brokenj.typechecker.{
    ProgramLabelNameCheckerComponent => _,
    MethodDefLabelNameCheckerComponent => _,
    _}
import sana.ooj.typechecker._
import sana.arrayj.typechecker._



trait LabelNameCheckerFamily extends CheckerFamily[(Tree, List[LabelApi])] {
  self =>

  override def default: Unit = ()

  def components: List[PartialFunction[(Tree, List[LabelApi]), Unit]] =
    generateComponents[(Tree, List[LabelApi]), Unit](
      Nodes.nodes,
      "LabelNameCheckerComponent", "check", "")

  def check: ((Tree, List[LabelApi])) => Unit = family
}

object LabelNameCheckerFamily extends LabelNameCheckerFamily



