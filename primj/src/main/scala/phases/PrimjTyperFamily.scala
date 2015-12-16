package ch.usi.inf.l3.sana.primj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.primj.PrimjNodes
import sana.primj.typechecker._
import sana.calcj.typechecker.{UnaryTyperComponent => _, _}



trait PrimjTyperFamily extends
  TransformationFamily[(Tree, List[Symbol]), Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[(Tree, List[Symbol]), Tree]] =
    generateComponents[(Tree, List[Symbol]), Tree](PrimjNodes.nodes,
      "TyperComponent", "typed", "")

  def typed: ((Tree, List[Symbol])) => Tree = family
}

object PrimjTyperFamily extends PrimjTyperFamily



