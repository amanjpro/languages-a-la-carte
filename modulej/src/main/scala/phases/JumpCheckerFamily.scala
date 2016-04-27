package ch.usi.inf.l3.sana.modulej.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.modulej.Nodes
import sana.brokenj.typechecker.{
    ProgramJumpCheckerComponent => _,
    MethodDefJumpCheckerComponent => _,
    _}
import sana.ooj.typechecker._
import sana.arrayj.typechecker._
import sana.robustj.typechecker._
import sana.ppj.typechecker._
import sana.dynj.typechecker._
import sana.modulej.typechecker._


trait JumpCheckerFamilyApi extends CheckerFamily[(Tree, List[Tree])] {
  self =>

  override def default = { case s => () }

  def components: List[PartialFunction[(Tree, List[Tree]), Unit]] =
    generateComponents[(Tree, List[Tree]), Unit](
      Nodes.nodes,
      "JumpCheckerComponent", "check", "Import,Throw")

  def check: ((Tree, List[Tree])) => Unit = family
}

case class JumpCheckerFamily(compiler: CompilerInterface)
  extends JumpCheckerFamilyApi



