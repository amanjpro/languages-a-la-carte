package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.ooj.OojNodes
import sana.primj.typechecker.{MethodDefTyperComponent => _,
                               ApplyTyperComponent => _,
                               TypeUseTyperComponent => _,
                               ValDefTyperComponent => _,
                               IdentTyperComponent => _, _}
import sana.calcj.typechecker.{UnaryTyperComponent => _,
                               BinaryTyperComponent => _,
                               _}
import sana.brokenj.typechecker._
import sana.ooj.typechecker._



trait TyperFamily extends TransformationFamily[(Tree, List[Symbol]), Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[(Tree, List[Symbol]), Tree]] =
    generateComponents[(Tree, List[Symbol]), Tree](OojNodes.nodes,
      "TyperComponent", "typed", "")

  def typed: ((Tree, List[Symbol])) => Tree = family
}

object TyperFamily extends TyperFamily



