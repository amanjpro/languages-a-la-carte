package ch.usi.inf.l3.sana.guod.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.modulej.Nodes
import sana.guod.codegen._

trait QualifierFamilyApi extends TransformationFamily[Tree, Tree] {
  self =>

  override def default = { case s => s }

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](Nodes.nodes,
      "QualifierComponent", "fullyqualify", "Literal,Import,This,Super,Break,Continue")

  def fullyqualify: Tree => Tree = family
}

case class QualifierFamily(compiler: CompilerInterface) extends QualifierFamilyApi



