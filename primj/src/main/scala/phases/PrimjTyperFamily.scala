package ch.usi.inf.l3.sana.primj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.primj.PrimjNodes
import sana.primj.typechecker._
import sana.calcj.typechecker.{UnaryTyperComponent => _, _}



trait PrimjTyperFamily extends
  TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](PrimjNodes.nodes,
      "TyperComponent", "typed", "")

  def typed: Tree => Tree = family
}

object PrimjTyperFamily extends PrimjTyperFamily



