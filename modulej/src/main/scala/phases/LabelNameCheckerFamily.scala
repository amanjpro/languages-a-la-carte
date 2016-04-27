package ch.usi.inf.l3.sana.modulej.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.brokenj.ast.{LabelApi}
import sana.modulej.Nodes
import sana.brokenj.typechecker.{
    ProgramLabelNameCheckerComponent => _,
    MethodDefLabelNameCheckerComponent => _,
    _}
import sana.ooj.typechecker._
import sana.arrayj.typechecker._
import sana.robustj.typechecker._
import sana.ppj.typechecker._
import sana.dynj.typechecker._
import sana.modulej.typechecker._



trait LabelNameCheckerFamilyApi extends CheckerFamily[(Tree, List[LabelApi])] {
  self =>

  override def default = { case s => () }

  def components: List[PartialFunction[(Tree, List[LabelApi]), Unit]] =
    generateComponents[(Tree, List[LabelApi]), Unit](
      Nodes.nodes,
      "LabelNameCheckerComponent", "check", "Import,Throw")

  def check: ((Tree, List[LabelApi])) => Unit = family
}

case class LabelNameCheckerFamily(compiler: CompilerInterface)
  extends LabelNameCheckerFamilyApi



