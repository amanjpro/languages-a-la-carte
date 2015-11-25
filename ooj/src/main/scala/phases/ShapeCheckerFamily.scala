package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.ooj.OojNodes
import sana.primj.typechecker._
import sana.calcj.typechecker._
import sana.ooj.typechecker._
import sana.brokenj.typechecker._



trait ShapeCheckerFamily extends CheckerFamily[Tree] {
  self =>

  override def default: Unit = ()

  def components: List[PartialFunction[Tree, Unit]] =
    generateComponents[Tree, Unit](OojNodes.nodes,
      "ShapeCheckerComponent", "check",
      """Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal,
      CompilationUnit,PackageDef,ClassDef,Template,Select,This,
      New,Super,Break,Continue
      """)

  def check: Tree => Unit = family
}

object ShapeCheckerFamily extends ShapeCheckerFamily


