package ch.usi.inf.l3.sana.dcct.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.primj.PrimjNodes
import sana.primj.namers._
import sana.ooj.namers.{TemplateSymbolAssignerComponent, ClassDefSymbolAssignerComponent, ClassDefNamerComponent, TemplateNamerComponent}



trait DcctNamerFamily extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](
      "Program,ClassDef,Template,ValDef,TypeUse,Ident",
      "NamerComponent", "name", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def name: Tree => Tree = family
}

object DcctNamerFamily extends DcctNamerFamily



