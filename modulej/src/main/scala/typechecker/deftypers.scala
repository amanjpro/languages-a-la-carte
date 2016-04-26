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
