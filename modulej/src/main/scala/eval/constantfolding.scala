package ch.usi.inf.l3.sana.modulej.eval


import ch.usi.inf.l3.sana
import sana.ooj
import sana.modulej
import sana.tiny
import sana.primj
import sana.calcj

import tiny.core.{TransformationComponent, CompilerInterface}
import tiny.dsl._


import modulej.ast._
import modulej.ast.Implicits._
import modulej.symbols.Implicits._
import modulej.modifiers.Ops._
import ooj.ast.{PackageDefApi, CompilationUnitApi => OCompilationUnitApi}
import tiny.ast.{UseTree, IdentApi, TypeUseApi, NoTree}
import primj.ast.ValDefApi
import primj.symbols.VariableSymbol
import calcj.ast.LiteralApi
import ooj.eval.ConstantFoldingComponent




@component(tree, env)
trait CompilationUnitConstantFoldingComponent extends
    ConstantFoldingComponent {
  (cunit: OCompilationUnitApi)  => {
    cunit match {
      case cunit: CompilationUnitApi     =>
        val (module, newEnv) = constantFold((cunit.module, env))
        module.bottomUp(())((_, y) => y match {
          case v: ValDefApi    if v.mods.isFinal && !v.mods.isField &&
                                 v.rhs != NoTree    =>
            for {
              s <- v.symbol
              o <- s.owner
            } {
              o.delete(s)
            }
          case _                                    =>
            ()
        })
        (TreeCopiers.copyCompilationUnit(cunit)(module =
            module.asInstanceOf[PackageDefApi]), newEnv)
      case cunit: OCompilationUnitApi    =>
        val res = TreeUpgraders.upgradeCompilationUnit(cunit)
        constantFold((res, env))
    }
  }
}

@component(tree, env)
trait ImportConstantFoldingComponent extends
  ConstantFoldingComponent {
  (imprt: ImportApi) => ((imprt, env))
}

@component(tree, env)
trait TypeUseConstantFoldingComponent
  extends ooj.eval.TypeUseConstantFoldingComponent {

  override protected def nameTypeUse(tuse: TypeUseApi): UseTree =
    typeUseNamer.nameTypeUse(tuse)

  private[this] val typeUseNamer = {
    val comp = this
    new modulej.namers.TypeUseNamer {
      protected val compiler: CompilerInterface = comp.compiler
      def family(use: UseTree): UseTree = compiler.typeCheck(
        use.owner)(use).asInstanceOf[UseTree]
    }
  }

}

@component(tree, env)
trait IdentConstantFoldingComponent
  extends ooj.eval.IdentConstantFoldingComponent {

  (id: IdentApi) => {
    val (res, env2) = super.apply((id, env))
    res match {
      case lit: LiteralApi              =>
        (lit, env2)
      case other                        =>
        other.symbol match {
          case Some(vsym: VariableSymbol)  if vsym.mods.isCompiled     =>
            vsym.compiledRHSLiteral match {
              case Some(v)              => (v, env2)
              case _                    => (res, env)
            }
          case _                                                       =>
            (res, env2)
        }
    }
  }

  override protected def nameIdent(id: IdentApi): UseTree =
    identNamer.nameIdent(id)

  override protected def typeAndNameIdent(id: IdentApi): UseTree =
    identNamer.nameIdent(id, false)

  private[this] val identNamer = {
    val comp = this
    new modulej.namers.IdentNamer with ooj.typechecker.IdentNamer {
      protected val compiler: CompilerInterface = comp.compiler
      def family(use: UseTree): UseTree = compiler.typeCheck(
        use.owner)(use).asInstanceOf[UseTree]
    }
  }

}
