package ch.usi.inf.l3.sana.arrooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.arrooj.Nodes
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
import sana.brokenj.typechecker.{LabelShapeCheckerComponent => _, _}
import sana.ooj.typechecker.{ClassDefShapeCheckerComponent => _,
                             MethodDefShapeCheckerComponent => _,
                             ValDefShapeCheckerComponent => _,
                             BlockShapeCheckerComponent => _,
                             LabelShapeCheckerComponent => _,
                             IfShapeCheckerComponent => _,
                             ForShapeCheckerComponent => _,
                             WhileShapeCheckerComponent => _,
                             CastShapeCheckerComponent => _,
                             _}
import sana.arrayj.typechecker.{MethodDefShapeCheckerComponent => _,
                                ArrayInitializerShapeCheckerComponent => _,
                                ArrayCreationShapeCheckerComponent => _,
                                LabelShapeCheckerComponent => _,
                                ValDefShapeCheckerComponent => _,
                                BlockShapeCheckerComponent => _,
                                IfShapeCheckerComponent => _,
                                ForShapeCheckerComponent => _,
                                WhileShapeCheckerComponent => _,
                                CastShapeCheckerComponent => _,
                                _}
import sana.arrooj.typechecker._



trait ShapeCheckerFamilyApi extends CheckerFamily[Tree] {
  self =>

  override def default: Unit = ()

  def components: List[PartialFunction[Tree, Unit]] =
    generateComponents[Tree, Unit](Nodes.nodes,
      "ShapeCheckerComponent", "check",
      """Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal,
      Select,This,New,Super,Break,Continue, ArrayCreation, ArrayAccess,
      ArrayTypeUse
      """)

  def check: Tree => Unit = family
}

case class ShapeCheckerFamily(compiler: CompilerInterface)
  extends ShapeCheckerFamilyApi



