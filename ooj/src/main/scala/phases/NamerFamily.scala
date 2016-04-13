package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.ooj.OojNodes
import sana.primj.namers.{MethodDefNamerComponent => _,
                          IdentNamerComponent => _,
                          ProgramNamerComponent => _, _}
import sana.brokenj.namers._
import sana.ooj.namers._



trait NamerFamilyApi extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](
      """Program,ClassDef,PackageDef,CompilationUnit,MethodDef,
          ValDef,TypeUse,Ident,Template,Select,Block""",
      "NamerComponent", "name", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def name: Tree => Tree = family
}

case class NamerFamily(compiler: CompilerInterface) extends NamerFamilyApi



