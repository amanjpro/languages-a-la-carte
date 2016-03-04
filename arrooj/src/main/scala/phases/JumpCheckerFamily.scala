package ch.usi.inf.l3.sana.arrooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.arrooj.Nodes
import sana.brokenj.typechecker.{
    ProgramJumpCheckerComponent => _,
    MethodDefJumpCheckerComponent => _,
    _}
import sana.ooj.typechecker._
import sana.arrayj.typechecker._



trait JumpCheckerFamily extends CheckerFamily[(Tree, List[Tree])] {
  self =>

  override def default: Unit = ()

  def components: List[PartialFunction[(Tree, List[Tree]), Unit]] =
    generateComponents[(Tree, List[Tree]), Unit](
      Nodes.nodes,
      "JumpCheckerComponent", "check", "")

  def check: ((Tree, List[Tree])) => Unit = family
}

object JumpCheckerFamily extends JumpCheckerFamily



