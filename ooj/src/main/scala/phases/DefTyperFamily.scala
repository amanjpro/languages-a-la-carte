package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.ooj.OojNodes
import sana.ooj.typechecker._



trait DefTyperFamilyApi extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](
      "Program,CompilationUnit,PackageDef,ClassDef,Template,MethodDef,ValDef,TypeUse,Select,Ident,Block",
      "DefTyperComponent", "typed", "")

  def typed: Tree => Tree = family
}

case class DefTyperFamily(compiler: CompilerInterface)
  extends DefTyperFamilyApi



