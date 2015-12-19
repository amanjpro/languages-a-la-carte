package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.ooj.OojNodes
import sana.ooj.typechecker._



trait TypeTyperFamily extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree]("Program,ClassDef,PackageDef,CompilationUnit",
      "TypeTyperComponent", "typed", "")

  def typed: Tree => Tree = family
}

object TypeTyperFamily extends TypeTyperFamily



