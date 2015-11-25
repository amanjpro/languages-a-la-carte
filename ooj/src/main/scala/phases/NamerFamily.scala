package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.ooj.OojNodes
import sana.primj.namers.{MethodDefNamerComponent => _,
                          IdentNamerComponent => _, _}
import sana.brokenj.namers._
import sana.ooj.namers._



trait NamerFamily extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](OojNodes.nodes,
      "NamerComponent", "name", "New")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def name: Tree => Tree = family
}

object NamerFamily extends NamerFamily



