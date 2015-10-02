package ch.usi.inf.l3.sana.primj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast.{Tree, NoTree}
import sana.primj.PrimjNodes
import sana.primj.namers._
// import sana.primj.namers.symbolassigners.Implicits._




trait PrimjSymbolAssignerFamily
  extends TransformationFamily[AssignerInput, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[AssignerInput, Tree]] =
    generateComponents[AssignerInput, Tree](PrimjNodes.nodes,
      "SymbolAssignerComponent", "assign", "Literal")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def assign: AssignerInput => Tree = family
}

object PrimjSymbolAssignerFamily extends PrimjSymbolAssignerFamily
