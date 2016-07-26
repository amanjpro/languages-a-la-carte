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

package ch.usi.inf.l3.sana.modulej.typechecker

import ch.usi.inf.l3.sana
import sana.modulej
import sana.ooj
import sana.arrooj
import sana.calcj
import sana.tiny
import sana.ppj
import sana.robustj

import tiny.core.{CompilerInterface, TransformationComponent}
import tiny.dsl._


import modulej.ast._
import modulej.ast.Implicits._
import ooj.ast.{PackageDefApi, SelectApi,
                CompilationUnitApi => OCompilationUnitApi}
import tiny.ast.{IdentApi, TypeUseApi, UseTree, NoTree, Tree}
import robustj.ast.{MethodDefApi}
import tiny.errors.ErrorReporting.{error,warning}
import modulej.errors.ErrorCodes._
import modulej.modifiers.Ops._
import calcj.typechecker.TyperComponent


// @component
// trait ImportTyperComponent extends TyperComponent {
//   (imprt: ImportApi) => {
//     val qual = typed(imprt.qual).asInstanceOf[UseTree]
//     qual.symbol match {
//       case None       if !imprt.isOnDemand                     =>
//         error(IMPORTED_PACKAGE_IS_MISSING,
//           "", "", qual.pos)
//       case None                                                =>
//         error(IMPORTED_CLASS_IS_MISSING,
//           "", "", qual.pos)
//       case _                                                   =>
//         ()
//     }
//     TreeCopiers.copyImport(imprt)(qual = qual)
//   }
// }

@component
trait CompilationUnitTyperComponent extends TyperComponent {
  (unit: OCompilationUnitApi) => {
    unit match {
      case unit: CompilationUnitApi       =>
        val pkg     = typed(unit.module).asInstanceOf[PackageDefApi]
        // val imports = unit.imports.map(typed(_).asInstanceOf[ImportApi])
        TreeCopiers.copyCompilationUnit(unit)(module = pkg)
      case unit: OCompilationUnitApi      =>
        val res = TreeUpgraders.upgradeCompilationUnit(unit)
        typed(res)
    }
  }
}


@component
trait IdentTyperComponent extends ooj.typechecker.IdentTyperComponent {
  (id: IdentApi)          => {
    attachQualifiedNameAttribute(id)
    super.apply(id)
  }
  /** @see [[ooj.typechecker.IdentTyperComponent.nameIdent]] */
  override protected def nameIdent(id: IdentApi): UseTree =
    identNamer.nameIdent(id)

  /** @see [[ooj.typechecker.IdentTyperComponent.typeAndNameIdent]] */
  override protected def typeAndNameIdent(id: IdentApi): UseTree =
    identNamer.nameIdent(id, true)


  /** @see [[ooj.typechecker.IdentTyperComponent.identNamer]] */
  private[this] val identNamer = {
    val comp = this
    new modulej.namers.IdentNamer with ooj.typechecker.IdentNamer {
      protected val compiler: CompilerInterface = comp.compiler
      def family(use: UseTree): UseTree = comp.typed(use).asInstanceOf[UseTree]
    }
  }

  /** @see {{{TreeUtils.attachQualifiedNameAttribute}}} */
  protected def attachQualifiedNameAttribute(use: UseTree): Unit =
    TreeUtils.attachQualifiedNameAttribute(use)
}

@component
trait TypeUseTyperComponent
  extends ooj.typechecker.TypeUseTyperComponent {
  (tuse: TypeUseApi) => {
    attachQualifiedNameAttribute(tuse)
    val tuseCopy = if(!tuse.hasBeenNamed) {
      typeUseNamer.nameTypeUse(tuse)
    } else tuse
    tuseCopy match {
      case tuse: TypeUseApi              =>
        super.apply(tuseCopy)
      case _                             =>
        typed(tuseCopy)
    }
  }

  /** @see [[ooj.typechecker.TypeUseTyperComponent.nameTypeUse]] */
  override protected def nameTypeUse(tuse: TypeUseApi): UseTree =
    typeUseNamer.nameTypeUse(tuse)

  /** @see [[ooj.typechecker.TypeUseTyperComponent.typeUseNamer]] */
  private[this] val typeUseNamer = {
    val comp = this
    new modulej.namers.TypeUseNamer {
      protected val compiler: CompilerInterface = comp.compiler
      def family(use: UseTree): UseTree =
        comp.typed(use).asInstanceOf[UseTree]
    }
  }

  /** @see {{{TreeUtils.attachQualifiedNameAttribute}}} */
  protected def attachQualifiedNameAttribute(use: UseTree): Unit =
    TreeUtils.attachQualifiedNameAttribute(use)
}


@component
trait SelectTyperComponent extends arrooj.typechecker.SelectTyperComponent {
  (slct: SelectApi) => {
    attachQualifiedNameAttribute(slct)
    super.apply(slct)
  }

  /** @see {{{TreeUtils.attachQualifiedNameAttribute}}} */
  protected def attachQualifiedNameAttribute(use: UseTree): Unit =
    TreeUtils.attachQualifiedNameAttribute(use)
}



@component
trait MethodDefTyperComponent extends
    ppj.typechecker.MethodDefTyperComponent {

  (mthd: MethodDefApi) => {
    val mthd2 = super.apply(mthd).asInstanceOf[MethodDefApi]

    if(mthd2.mods.isNative && !isConstructor(mthd.symbol) &&
          mthd2.body != NoTree) {
      error(NATIVE_METHOD_CANNOT_HAVE_BODY,
          "", "", mthd.pos)
    }

    if(mthd2.mods.isNative && isConstructor(mthd.symbol)) {
      error(CONSTRUCTOR_CANNOT_BE_NATIVE,
          "", "", mthd.pos)
    }


    mthd2
  }

  /** @see [[ppj.typechecker.MethodDefTyperComponent.allPathsReturn]] */
  override def allPathsReturn(expr: Tree): Boolean = {
    enclosingMethod(expr.symbol) match {
      case Some(mthd)                         =>
        mthd.mods.isNative || super.allPathsReturn(expr)
      case None                               =>
        super.allPathsReturn(expr)
    }
  }
}
