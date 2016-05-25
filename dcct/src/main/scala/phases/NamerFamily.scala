package ch.usi.inf.l3.sana.dcct.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.primj.PrimjNodes
import sana.primj.namers._
import sana.ooj.namers.TemplateNamerComponent
import sana.dcct.namers.ClassDefNamerComponent



trait DcctNamerFamilyApi extends TransformationFamily[Tree, Tree] {
  self =>

  override def default = {case s => s}

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](
      "Program,ClassDef,Template,ValDef,TypeUse,Ident",
      "NamerComponent", "name", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def name: Tree => Tree = family
}

case class DcctNamerFamily(compiler: CompilerInterface)
  extends DcctNamerFamilyApi



