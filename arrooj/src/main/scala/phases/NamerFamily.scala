package ch.usi.inf.l3.sana.arrooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.arrooj.Nodes
import sana.primj.namers.{MethodDefNamerComponent => _,
                          IdentNamerComponent => _,
                          ProgramNamerComponent => _, _}
import sana.brokenj.namers._
import sana.ooj.namers._
import sana.arrooj.namers._



trait NamerFamily extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](
      """Program,ClassDef,PackageDef,CompilationUnit,MethodDef,
          ValDef,TypeUse,Ident,Template,Select,Block,ArrayTypeUse""",
      "NamerComponent", "name", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def name: Tree => Tree = family
}

object NamerFamily extends NamerFamily



