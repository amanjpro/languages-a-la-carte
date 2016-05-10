package ch.usi.inf.l3.sana.oberon0.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast.{Tree, NoTree}
import sana.oberon0.Nodes
import sana.oberon0.codegen._


trait CodeGenFamilyApi
  extends TransformationFamily[(Tree, Int), String] {
  self =>

  override def default = { case s => s"\n${tab(s._2)}{}" }

  def components: List[PartialFunction[(Tree, Int), String]] =
    generateComponents[(Tree, Int), String](Nodes.nodes,
      "CodeGenComponent", "codegen", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def codegen: ((Tree, Int)) => String = family
}

case class CodeGenFamily(compiler: CompilerInterface)
  extends CodeGenFamilyApi
