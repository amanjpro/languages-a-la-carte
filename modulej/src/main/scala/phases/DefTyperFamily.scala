package ch.usi.inf.l3.sana.modulej.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.ooj.typechecker.{SelectDefTyperComponent => _,
                             CompilationUnitDefTyperComponent => _,
                             MethodDefDefTyperComponent => _, _}
import sana.arrooj.typechecker._
import sana.robustj.typechecker._
import sana.ppj.typechecker._
import sana.dynj.typechecker._
import sana.modulej.typechecker._



trait DefTyperFamilyApi extends TransformationFamily[Tree, Tree] {
  self =>

  override def default = { case s => s }

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](
      "Program,CompilationUnit,PackageDef,ClassDef,Template,MethodDef,ValDef,TypeUse,Select,Ident,Block,ArrayTypeUse,Import",
      "DefTyperComponent", "typed", "")

  def typed: Tree => Tree = family
}

case class DefTyperFamily(compiler: CompilerInterface) extends DefTyperFamilyApi



