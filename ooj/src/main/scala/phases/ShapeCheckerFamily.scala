package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.ooj.OojNodes
import sana.primj.typechecker.{MethodDefShapeCheckerComponent => _,
                               ValDefShapeCheckerComponent => _,
                               BlockShapeCheckerComponent => _,
                               IfShapeCheckerComponent => _,
                               ForShapeCheckerComponent => _,
                               WhileShapeCheckerComponent => _,
                               CastShapeCheckerComponent => _,
                               ProgramShapeCheckerComponent => _,
                               _}
import sana.calcj.typechecker._
import sana.ooj.typechecker._
import sana.brokenj.typechecker.{LabelShapeCheckerComponent => _, _}



trait ShapeCheckerFamilyApi extends CheckerFamily[Tree] {
  self =>

  override def default: Unit = ()

  def components: List[PartialFunction[Tree, Unit]] =
    generateComponents[Tree, Unit](OojNodes.nodes,
      "ShapeCheckerComponent", "check",
      """Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal,
      Select,This,New,Super,Break,Continue
      """)

  def check: Tree => Unit = family
}

case class ShapeCheckerFamily(compiler: CompilerInterface)
  extends ShapeCheckerFamilyApi



