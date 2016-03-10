package ch.usi.inf.l3.sana.arrooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.arrooj.Nodes
import sana.ooj.eval._



trait ConstantCollectingFamily
  extends TransformationFamily[(Tree, Env), (Tree, Env)] {
  self =>

  override def default: (Tree, Env) = (NoTree, Env.emptyEnv)

  def components: List[PartialFunction[(Tree, Env), (Tree, Env)]] =
    generateComponents[(Tree, Env), (Tree, Env)](
      "Program,CompilationUnit,PackageDef,ClassDef,ValDef,MethodDef,Block,Template",
      "ConstantCollectingComponent", "collect", "")

  def collect: ((Tree, Env)) => (Tree, Env) = family
}

object ConstantCollectingFamily extends ConstantCollectingFamily


