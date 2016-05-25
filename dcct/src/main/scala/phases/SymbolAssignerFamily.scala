package ch.usi.inf.l3.sana.dcct.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast.{Tree, NoTree}
import sana.dcct.DCCTNodes
import sana.primj.namers._
import sana.ooj.namers.TemplateSymbolAssignerComponent
import sana.dcct.namers._


trait DcctSymbolAssignerFamilyApi
  extends TransformationFamily[Tree, Tree] {
  self =>

  override def default = {case s => s}

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](DCCTNodes.nodes,
      "SymbolAssignerComponent", "assign", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def assign: Tree => Tree = family
}

case class DcctSymbolAssignerFamily(compiler: CompilerInterface)
  extends DcctSymbolAssignerFamilyApi
