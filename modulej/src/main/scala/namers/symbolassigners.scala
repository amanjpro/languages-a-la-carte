/*
 * Copyright (c) <2015-2016>, see CONTRIBUTERS
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

package ch.usi.inf.l3.sana.modulej.namers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.robustj
import sana.ooj
import sana.modulej

import tiny.dsl._
import primj.namers.SymbolAssignerComponent
import ooj.symbols.{PackageSymbol, ClassSymbol}
import tiny.symbols.Symbol
import tiny.source.Position
import tiny.names.Name
import modulej.ast.Implicits._
import modulej.modifiers.Ops._
import modulej.symbols.Implicits._
import modulej.ast._
import primj.ast.ValDefApi
import calcj.ast.LiteralApi
import primj.symbols.VariableSymbol
import ooj.ast.{PackageDefApi, ClassDefApi,
                CompilationUnitApi => OCompilationUnitApi}
import robustj.names.StdNames._
import modulej.symbols.{CompilationUnitSymbol, SymbolUtils}
import tiny.ast.{NoTree, UseTree}

@component
trait CompilationUnitSymbolAssignerComponent extends SymbolAssignerComponent {
  (cunit: OCompilationUnitApi) => {
    cunit match {
      case cunit: CompilationUnitApi       =>
        cunit.owner.foreach(cunit.module.owner = _)

        val sym     = CompilationUnitSymbol(Nil, None, cunit.sourceName,
                                      cunit.sourcePath, None)
        cunit.module.owner = sym
        val pkg     = assign(cunit.module).asInstanceOf[PackageDefApi]
        val owner   = pkg.symbol
        sym.owner   = owner
        owner.foreach(owner => {
          cunit.owner = owner
          owner.declare(sym)
        })
        cunit.symbol       = sym
        sym.module       = Some(sym)
        cunit.imports.foreach(im => im.owner = sym)
        val imports        =
          cunit.imports.map(assign(_).asInstanceOf[ImportApi])
        TreeCopiers.copyCompilationUnit(cunit)(imports = imports,
          module = pkg)
      case cunit: OCompilationUnitApi      =>
        val res = TreeUpgraders.upgradeCompilationUnit(cunit)
        assign(res)
    }
  }

  protected def rootSymbol: Option[Symbol] =
    SymbolUtils.rootSymbol
}

@component
trait ImportSymbolAssignerComponent extends SymbolAssignerComponent {
  (imprt: ImportApi)     => {
    imprt.owner.foreach(owner => imprt.qual.owner = owner)
    val qual = assign(imprt.qual).asInstanceOf[UseTree]
    TreeCopiers.copyImport(imprt)(qual = qual)
  }
}

@component
trait ValDefSymbolAssignerComponent
  extends ooj.namers.ValDefSymbolAssignerComponent {
  (valdef: ValDefApi) => {
    val res = super.apply(valdef).asInstanceOf[ValDefApi]
    res.symbol match {
      case Some(vsym: VariableSymbol)   if valdef.mods.isCompiled =>
        valdef.rhs match {
          case lit: LiteralApi     =>
            vsym.compiledRHSLiteral = lit
          case _                   => ()
        }
      case _                                                      =>
        ()
    }
    res
  }
}

// @component
// trait PackageDefSymbolAssignerComponent
//   extends ooj.namers.PackageDefSymbolAssignerComponent {
//   (pkg: PackageDefApi) => {
//     val pkg2    = super.apply(pkg).asInstanceOf[PackageDefApi]
//     val members = pkg2.members.filter(_ != NoTree)
//     TreeCopiers.copyPackageDef(pkg2)(members = members)
//   }
// }
//
// @component
// trait ClassDefSymbolAssignerComponent
//   extends ooj.namers.ClassDefSymbolAssignerComponent {
//
//   (clazz: ClassDefApi) => {
//     val res = clazz.owner.map { owner =>
//       if(owner.directlyDefinesName(clazz.name,
//         _.isInstanceOf[ClassSymbol]) &&
//         clazz.mods.isCompiled) NoTree
//       else super.apply(clazz)
//     }
//     res.getOrElse(super.apply(clazz))
//   }
  // override protected def createClassSymbol(clazz: ClassDefApi,
  //   owner: Option[Symbol]): ClassSymbol = {
  //   owner match {
  //     case Some(`langPackageSymbol`)   if isBuiltInName(clazz.name)   =>
  //       getBuiltInSymbol(clazz.name)
  //     case _                                                          =>
  //       super.createClassSymbol(clazz, owner)
  //   }
  // }
  //
  // override protected def classDoubleDefCheck(owner: Option[Symbol],
  //   name: Name, pos: Option[Position]): Unit =
  //   if(!isBuiltInName(name)) super.classDoubleDefCheck(owner, name, pos)
  //
  // protected def isBuiltInName(name: Name): Boolean = {
  //   name == OBJECT_TYPE_NAME ||
  //     name == STRING_TYPE_NAME ||
  //     name == BOOLEAN_CLASS_NAME ||
  //     name == CHARACTER_CLASS_NAME ||
  //     name == INTEGER_CLASS_NAME ||
  //     name == LONG_CLASS_NAME ||
  //     name == FLOAT_CLASS_NAME ||
  //     name == DOUBLE_CLASS_NAME ||
  //     name == THROWABLE_CLASS_NAME ||
  //     name == EXCEPTION_CLASS_NAME ||
  //     name == RUNTIME_EXCEPTION_CLASS_NAME ||
  //     name == ERROR_CLASS_NAME
  // }
  //
  // protected def getBuiltInSymbol(name: Name): ClassSymbol = {
  //   if(name == OBJECT_TYPE_NAME)
  //     SymbolUtils.objectClassSymbol
  //   else if(name == STRING_TYPE_NAME)
  //     SymbolUtils.stringClassSymbol
  //   else if(name == BOOLEAN_CLASS_NAME)
  //     SymbolUtils.booleanClassSymbol
  //   else if(name == CHARACTER_CLASS_NAME)
  //     SymbolUtils.characterClassSymbol
  //   else if(name == INTEGER_CLASS_NAME)
  //     SymbolUtils.integerClassSymbol
  //   else if(name == LONG_CLASS_NAME)
  //     SymbolUtils.longClassSymbol
  //   else if(name == FLOAT_CLASS_NAME)
  //     SymbolUtils.floatClassSymbol
  //   else if(name == DOUBLE_CLASS_NAME)
  //     SymbolUtils.doubleClassSymbol
  //   else if(name == THROWABLE_CLASS_NAME)
  //     SymbolUtils.throwableClassSymbol
  //   else if(name == EXCEPTION_CLASS_NAME)
  //     SymbolUtils.exceptionClassSymbol
  //   else if(name == RUNTIME_EXCEPTION_CLASS_NAME)
  //     SymbolUtils.runtimeExceptionClassSymbol
  //   else // name == ERROR_CLASS_NAME
  //     SymbolUtils.errorClassSymbol
  // }
  // protected val langPackageSymbol: PackageSymbol =
  //   SymbolUtils.langPackageSymbol
// }
