package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.ooj.OojNodes
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
                               LiteralTyperComponent => _,
                               _}
import sana.brokenj.typechecker._
import sana.ooj.typechecker._



trait TyperFamilyApi extends TransformationFamily[Tree, Tree] {
  self =>

  override def default = { case s => s }

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](OojNodes.nodes,
      "TyperComponent", "typed", "")

  def typed: Tree => Tree = family
}

case class TyperFamily(compiler: CompilerInterface) extends TyperFamilyApi



