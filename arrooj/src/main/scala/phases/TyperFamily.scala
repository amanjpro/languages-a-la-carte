package ch.usi.inf.l3.sana.arrooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.arrooj.Nodes
import sana.primj.typechecker.{MethodDefTyperComponent => _,
                               ApplyTyperComponent => _,
                               TypeUseTyperComponent => _,
                               ValDefTyperComponent => _,
                               AssignTyperComponent => _,
                               TernaryTyperComponent => _,
                               ProgramTyperComponent => _,
                               IdentTyperComponent => _, _}
import sana.calcj.typechecker.{UnaryTyperComponent => _,
                               BinaryTyperComponent => _,
                               _}
import sana.brokenj.typechecker._
import sana.ooj.typechecker.{SelectTyperComponent => _,
                             ValDefTyperComponent => _, _}
import sana.arrooj.typechecker._


trait TyperFamily extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](Nodes.nodes,
      "TyperComponent", "typed", "")

  def typed: Tree => Tree = family
}

object TyperFamily extends TyperFamily



