package ch.usi.inf.l3.sana.dcct.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast.{Tree, NoTree}
import sana.dcct.DCCTNodes
import sana.primj.PrimjNodes
import sana.primj.namers._
import sana.ooj.namers.ClassDefSymbolAssignerComponent


trait DcctSymbolAssignerFamily
  extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](DCCTNodes.nodes,
      "SymbolAssignerComponent", "assign", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def assign: Tree => Tree = family
}

object DcctSymbolAssignerFamily extends DcctSymbolAssignerFamily
