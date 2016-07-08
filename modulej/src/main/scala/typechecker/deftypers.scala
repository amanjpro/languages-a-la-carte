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
import sana.tiny

import tiny.core.TransformationComponent
import tiny.dsl._


import modulej.ast._
import ooj.ast.{PackageDefApi, CompilationUnitApi => OCompilationUnitApi}
import tiny.ast.UseTree
import ooj.typechecker.DefTyperComponent


@component
trait ImportDefTyperComponent extends DefTyperComponent {
  (imprt: ImportApi) => {
    val qual = typed(imprt.qual).asInstanceOf[UseTree]
    TreeCopiers.copyImport(imprt)(qual = qual)
  }
}

@component
trait CompilationUnitDefTyperComponent extends DefTyperComponent {
  (unit: OCompilationUnitApi) => {
    unit match {
      case unit: CompilationUnitApi       =>
        val pkg     = typed(unit.module).asInstanceOf[PackageDefApi]
        val imports = unit.imports.map(typed(_).asInstanceOf[ImportApi])
        TreeCopiers.copyCompilationUnit(unit)(imports = imports, module = pkg)
      case unit: OCompilationUnitApi      =>
        val res = TreeUpgraders.upgradeCompilationUnit(unit)
        typed(res)
    }
  }
}
