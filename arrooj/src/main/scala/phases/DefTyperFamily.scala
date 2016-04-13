package ch.usi.inf.l3.sana.arrooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.arrooj.Nodes
import sana.ooj.typechecker.{SelectDefTyperComponent => _, _}
import sana.arrooj.typechecker._



trait DefTyperFamily extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](
      "Program,CompilationUnit,PackageDef,ClassDef,Template,MethodDef,ValDef,TypeUse,Select,Ident,Block, ArrayTypeUse",
      "DefTyperComponent", "typed", "")

  def typed: Tree => Tree = family
}

object DefTyperFamily extends DefTyperFamily



