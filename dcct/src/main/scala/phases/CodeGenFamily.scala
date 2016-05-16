package ch.usi.inf.l3.sana.dcct.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.dcct.codegenerator._


trait DcctCodeGenFamilyApi extends TransformationFamily[Tree, String] {
  self =>

  override def default: String = ""

  def components: List[PartialFunction[Tree, String]] =
    generateComponents[Tree, String](
      "Program,Entity",
      "CodeGenComponent", "codegen", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def codegen: Tree => String = family 
}

case class DcctCodeGenFamily(compiler: CompilerInterface)
  extends DcctCodeGenFamilyApi



