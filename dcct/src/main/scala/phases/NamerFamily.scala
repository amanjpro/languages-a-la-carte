package ch.usi.inf.l3.sana.dcct.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.primj.PrimjNodes
import sana.primj.namers._
import sana.ooj.namers.TemplateNamerComponent
import sana.dcct.namers._


trait DcctNamerFamilyApi extends TransformationFamily[Tree, Tree] {
  self =>

  override def default = {case s => s}

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](
      "Program, MethodDef, ValDef, TypeUse, Ident, ClassDef, Template, ArrayDef",
      "NamerComponent", "name", "")

  def name: Tree => Tree = family 
}

case class DcctNamerFamily(compiler: CompilerInterface)
  extends DcctNamerFamilyApi



