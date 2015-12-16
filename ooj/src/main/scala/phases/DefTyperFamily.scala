package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.ooj.OojNodes
import sana.ooj.typechecker._



trait DefTyperFamily extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](
      "CompilationUnit,PackageDef,ClassDef,Template,MethodDef,ValDef,TypeUse,Select,Ident,Block",
      "DefTyperComponent", "typed", "")

  def typed: Tree => Tree = family
}

object DefTyperFamily extends DefTyperFamily



