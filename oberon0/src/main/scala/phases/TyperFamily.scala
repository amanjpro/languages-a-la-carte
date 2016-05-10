package ch.usi.inf.l3.sana.oberon0.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast.{Tree, NoTree}
import sana.oberon0.Nodes
import sana.oberon0.typechecker._
import sana.calcj.typechecker.{LiteralTyperComponent, BinaryTyperComponent,
                               UnaryTyperComponent}
import sana.primj.typechecker.{ValDefTyperComponent => _, _}
import sana.ooj.typechecker.{TemplateTyperComponent, SelectTyperComponent}
import sana.arrayj.typechecker.{ArrayAccessTyperComponent}


trait TyperFamilyApi
  extends TransformationFamily[Tree, Tree] {
  self =>

  override def default = { case s: Tree => s }

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](Nodes.nodes,
      "TyperComponent", "typed", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def typed: Tree => Tree = family
}

case class TyperFamily(compiler: CompilerInterface)
  extends TyperFamilyApi
