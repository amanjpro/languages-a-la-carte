package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.ooj.OojNodes
import sana.brokenj.typechecker.{
    ProgramJumpCheckerComponent => _,
    MethodDefJumpCheckerComponent => _,
    _}
import sana.ooj.typechecker._



trait JumpCheckerFamily extends CheckerFamily[(Tree, List[Tree])] {
  self =>

  override def default: Unit = ()

  def components: List[PartialFunction[(Tree, List[Tree]), Unit]] =
    generateComponents[(Tree, List[Tree]), Unit](
      OojNodes.nodes,
      "JumpCheckerComponent", "check", "")

  def check: ((Tree, List[Tree])) => Unit = family
}

object JumpCheckerFamily extends JumpCheckerFamily



