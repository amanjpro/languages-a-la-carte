package ch.usi.inf.l3.sana.guod.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.modulej.Nodes
import sana.guod.codegen._

trait LocalVariablesFamilyApi extends TransformationFamily[(Tree, Env), Unit] {
  self =>

  override def default = { case s => () }

  def components: List[PartialFunction[(Tree, Env), Unit]] =
    generateComponents[(Tree, Env), Unit](Nodes.nodes,
      "LocalVariablesComponent", "subst", "Import,This,Super,Literal,TypeUse,ArrayTypeUse,ArrayCreation,Break,Continue")

  def subst: ((Tree, Env)) => Unit = family
}

case class LocalVariablesFamily(compiler: CompilerInterface) extends LocalVariablesFamilyApi



