package ch.usi.inf.l3.sana.primj.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast.{Tree, NoTree}
import sana.primj.PrimjNodes
import sana.primj.namers._




trait PrimjSymbolAssignerFamilyApi
  extends TransformationFamily[Tree, Tree] {
  self =>

  override def default = { case s: Tree => s }

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](PrimjNodes.nodes,
      "SymbolAssignerComponent", "assign", "")
      
  def assign: Tree => Tree = family
}

case class PrimjSymbolAssignerFamily(compiler: CompilerInterface)
  extends PrimjSymbolAssignerFamilyApi
