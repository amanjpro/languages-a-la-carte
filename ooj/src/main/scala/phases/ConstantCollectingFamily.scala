package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.ooj.OojNodes
import sana.ooj.eval._



trait ConstantCollectingFamilyApi
  extends TransformationFamily[(Tree, Env), Env] {
  self =>

  override def default: Env = Env.emptyEnv

  def components: List[PartialFunction[(Tree, Env), Env]] =
    generateComponents[(Tree, Env), Env](
      "Program,CompilationUnit,PackageDef,ClassDef,ValDef,MethodDef,Block,Template",
      "ConstantCollectingComponent", "collect", "")

  def collect: ((Tree, Env)) => Env = family
}

case class ConstantCollectingFamily(compiler: CompilerInterface)
  extends ConstantCollectingFamilyApi



