package ch.usi.inf.l3.sana.modulej.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.modulej.Nodes
import sana.primj.typechecker.{MethodDefTyperComponent => _,
                               ApplyTyperComponent => _,
                               TypeUseTyperComponent => _,
                               ValDefTyperComponent => _,
                               UnaryTyperComponent => _,
                               AssignTyperComponent => _,
                               TernaryTyperComponent => _,
                               ProgramTyperComponent => _,
                               IdentTyperComponent => _, _}
import sana.calcj.typechecker.{UnaryTyperComponent => _,
                               BinaryTyperComponent => _,
                               CastTyperComponent => _,
                               LiteralTyperComponent => _,
                               _}
import sana.brokenj.typechecker._
import sana.ooj.typechecker.{SelectTyperComponent => _,
                             AssignTyperComponent => _,
                             TypeUseTyperComponent => _,
                             IdentTyperComponent => _,
                             UnaryTyperComponent => _,
                             MethodDefTyperComponent => _,
                             CompilationUnitTyperComponent => _,
                             BinaryTyperComponent => _,
                             ValDefTyperComponent => _, _}
import sana.arrooj.typechecker.{SelectTyperComponent => _, _}
import sana.robustj.typechecker.{MethodDefTyperComponent => _, _}
import sana.ppj.typechecker.{MethodDefTyperComponent => _, _}
import sana.dynj.typechecker._
import sana.modulej.typechecker._


trait TyperFamilyApi extends TransformationFamily[Tree, Tree] {
  self =>

  override def default = { case s => s }

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](Nodes.nodes,
      "TyperComponent", "typed", "Import")

  def typed: Tree => Tree = family
}

case class TyperFamily(compiler: CompilerInterface) extends TyperFamilyApi



