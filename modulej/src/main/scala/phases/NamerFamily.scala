package ch.usi.inf.l3.sana.modulej.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.primj.namers.{MethodDefNamerComponent => _,
                          IdentNamerComponent => _,
                          TypeUseNamerComponent => _,
                          ProgramNamerComponent => _, _}
import sana.brokenj.namers._
import sana.ooj.namers.{CompilationUnitNamerComponent => _,
                        MethodDefNamerComponent => _,
                        SelectNamerComponent => _,
                        ClassDefNamerComponent => _,
                        IdentNamerComponent => _, _}
import sana.arrooj.namers._
import sana.robustj.namers._
import sana.ppj.namers._
import sana.modulej.namers._




trait NamerFamilyApi extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](
      """Program,ClassDef,PackageDef,CompilationUnit,MethodDef,
          ValDef,TypeUse,Ident,Template,Select,Block,ArrayTypeUse,Import""",
      "NamerComponent", "name", "")

  def name: Tree => Tree = family
}

case class NamerFamily(compiler: CompilerInterface) extends NamerFamilyApi



