package ch.usi.inf.l3.sana.ooj.phases

import ch.usi.inf.l3.sana
import sana.dsl._
import sana.core._
import sana.core.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast.{Tree, NoTree}
import sana.ooj.OojNodes
import sana.primj.namers.{MethodDefSymbolAssignerComponent => _,
                          BlockSymbolAssignerComponent => _,
                          ValDefSymbolAssignerComponent => _, _}
import sana.brokenj.namers._
import sana.ooj.namers._




trait SymbolAssignerFamily
  extends TransformationFamily[AssignerInput, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[AssignerInput, Tree]] =
    generateComponents[AssignerInput, Tree](OojNodes.nodes,
      "SymbolAssignerComponent", "assign", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def assign: AssignerInput => Tree = family
}

object SymbolAssignerFamily extends SymbolAssignerFamily
