package ch.usi.inf.l3.sana.guod.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.modulej.Nodes
import sana.guod.codegen._

trait CodeGenFamilyApi extends TransformationFamily[(Tree, ByteCodeWriter), Unit] {
  self =>

  override def default = { case s => () }

  def components: List[PartialFunction[(Tree, ByteCodeWriter), Unit]] =
    generateComponents[(Tree, ByteCodeWriter), Unit](Nodes.nodes,
      "CodeGenComponent", "codegen", "Import,Catch,ArrayTypeUse,Case,TypeUse")

  def codegen: ((Tree, ByteCodeWriter)) => Unit = family
}

case class CodeGenFamily(compiler: CompilerInterface) extends CodeGenFamilyApi



