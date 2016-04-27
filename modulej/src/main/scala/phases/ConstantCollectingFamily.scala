package ch.usi.inf.l3.sana.modulej.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.ast.{Tree, NoTree}
import sana.tiny.symbols.Symbol
import sana.ooj.eval._



trait ConstantCollectingFamilyApi
  extends TransformationFamily[(Tree, Env), Env] {
  self =>

  override def default = { case s: ((Tree, Env)) => s._2 }

  def components: List[PartialFunction[(Tree, Env), Env]] =
    generateComponents[(Tree, Env), Env](
      "Program,CompilationUnit,PackageDef,ClassDef,ValDef,MethodDef,Block,Template",
      "ConstantCollectingComponent", "collect", "")

  def collect: ((Tree, Env)) => Env = family
}

case class ConstantCollectingFamily(compiler: CompilerInterface)
  extends ConstantCollectingFamilyApi



