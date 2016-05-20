package ch.usi.inf.l3.sana.guod.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.modulej.Nodes
import sana.guod.codegen._

trait InitializersFamilyApi extends TransformationFamily[Tree, Tree] {
  self =>

  override def default = { case s => s }

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree]("Program,PackageDef,ClassDef,Template,CompilationUnit",
      "InitializerComponent", "inline", "")

  def inline: Tree => Tree = family
}

case class InitializersFamily(compiler: CompilerInterface) extends InitializersFamilyApi



