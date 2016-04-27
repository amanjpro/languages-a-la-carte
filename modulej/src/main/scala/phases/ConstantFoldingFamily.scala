package ch.usi.inf.l3.sana.modulej.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.modulej.Nodes
import sana.ooj.eval.{CompilationUnitConstantFoldingComponent => _,
                      MethodDefConstantFoldingComponent => _,
                      TypeUseConstantFoldingComponent => _,
                      IdentConstantFoldingComponent => _, _}
import sana.arrooj.eval._
import sana.robustj.eval._
import sana.ppj.eval._
import sana.modulej.eval._



trait ConstantFoldingFamilyApi
  extends TransformationFamily[(Tree, Env), (Tree, Env)] {
  self =>

  override def default = { case s: ((Tree, Env)) => s }

  def components: List[PartialFunction[(Tree, Env), (Tree, Env)]] =
    generateComponents[(Tree, Env), (Tree, Env)](Nodes.nodes,
      "ConstantFoldingComponent", "constantFold", "")

  def constantFold: ((Tree, Env)) => (Tree, Env) = family
}

case class ConstantFoldingFamily(compiler: CompilerInterface)
  extends ConstantFoldingFamilyApi



