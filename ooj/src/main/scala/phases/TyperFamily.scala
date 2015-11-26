package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.ooj.OojNodes
import sana.primj.typechecker.{MethodDefTyperComponent => _, _}
import sana.calcj.typechecker.{UnaryTyperComponent => _, _}
import sana.brokenj.typechecker._
import sana.ooj.typechecker._



trait TyperFamily extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](OojNodes.nodes,
      "TyperComponent", "typed", "Select")

  def typed: Tree => Tree = family
}

object TyperFamily extends TyperFamily



