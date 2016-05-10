package ch.usi.inf.l3.sana.oberon0.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast.{Tree, NoTree}
import sana.oberon0.Nodes
import sana.oberon0.namers._
import sana.primj.namers._
import sana.ooj.namers.TemplateNamerComponent
// import sana.arrayj.namers.{ArrayAccessNamerComponent}


trait NamerFamilyApi
  extends TransformationFamily[Tree, Tree] {
  self =>

  override def default = { case s: Tree => s }

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](Nodes.nodes,
      "NamerComponent", "name", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def name: Tree => Tree = family
}

case class NamerFamily(compiler: CompilerInterface)
  extends NamerFamilyApi
