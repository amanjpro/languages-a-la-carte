package ch.usi.inf.l3.sana.dcct.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.dcct.codegenerator._


trait DcctCodeGenFamily extends TransformationFamily[Tree, String] {
  self =>

  override def default: String = ""

  def components: List[PartialFunction[Tree, String]] =
    generateComponents[Tree, String](
      "Program,Entity",
      "CodeGenComponent", "codegen", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def codegen: Tree => String = family
}

object DcctCodeGenFamily extends DcctCodeGenFamily



