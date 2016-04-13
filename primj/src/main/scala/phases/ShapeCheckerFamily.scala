package ch.usi.inf.l3.sana.primj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.primj.PrimjNodes
import sana.primj.typechecker._
import sana.calcj.typechecker._



trait PrimjShapeCheckerFamilyApi extends CheckerFamily[Tree] {
  self =>

  override def default: Unit = ()

  def components: List[PartialFunction[Tree, Unit]] =
    generateComponents[Tree, Unit](PrimjNodes.nodes,
      "ShapeCheckerComponent", "check",
      "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def check: Tree => Unit = family
}

case class PrimjShapeCheckerFamily(compiler: CompilerInterface)
  extends PrimjShapeCheckerFamilyApi



