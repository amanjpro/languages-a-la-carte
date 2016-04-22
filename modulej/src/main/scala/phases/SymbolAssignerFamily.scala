package ch.usi.inf.l3.sana.modulej.phases

import ch.usi.inf.l3.sana
import sana.tiny.dsl._
import sana.tiny.core._
import sana.tiny.core.Implicits._
import sana.tiny.symbols.Symbol
import sana.tiny.ast.{Tree, NoTree}
import sana.modulej.Nodes
import sana.primj.namers.{MethodDefSymbolAssignerComponent => _,
                          BlockSymbolAssignerComponent => _,
                          ProgramSymbolAssignerComponent => _,
                          ValDefSymbolAssignerComponent => _, _}
import sana.brokenj.namers._
import sana.ooj.namers.{CompilationUnitSymbolAssignerComponent => _,
                        MethodDefSymbolAssignerComponent => _, _}
                        // PackageDefSymbolAssignerComponent => _,
                        // ClassDefSymbolAssignerComponent => _, _}
import sana.arrayj.namers._
import sana.robustj.namers._
import sana.ppj.namers._
import sana.modulej.namers._




trait SymbolAssignerFamilyApi
  extends TransformationFamily[Tree, Tree] {
  self =>

  override def default: Tree = NoTree

  def components: List[PartialFunction[Tree, Tree]] =
    generateComponents[Tree, Tree](Nodes.nodes,
      "SymbolAssignerComponent", "assign", "")
      // "Ident,TypeUse,Assign,Ternary,Apply,Return,Binary,Literal")

  def assign: Tree => Tree = family
}

case class SymbolAssignerFamily(compiler: CompilerInterface)
  extends SymbolAssignerFamilyApi
