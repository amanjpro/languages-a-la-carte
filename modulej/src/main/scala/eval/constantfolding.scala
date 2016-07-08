/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
