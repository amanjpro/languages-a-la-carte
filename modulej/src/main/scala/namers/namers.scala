package ch.usi.inf.l3.sana.modulej.namers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import sana.modulej

import tiny.dsl._
import primj.namers.NamerComponent
import modulej.ast._
import ooj.ast.PackageDefApi
import modulej.symbols.CompilationUnitSymbol
import tiny.ast.UseTree
import modulej.ast.Implicits._


@component
trait CompilationUnitNamerComponent extends NamerComponent {
  (unit: CompilationUnitApi) => {
    val pkg     = name(unit.module).asInstanceOf[PackageDefApi]
    val imports = unit.imports.map(name(_).asInstanceOf[ImportApi])
    TreeCopiers.copyCompilationUnit(unit)(imports = imports, module = pkg)
  }
}

@component
trait ImportNamerComponent extends NamerComponent {
  (imprt: ImportApi)     => {
    imprt.qual.isImportQual = true
    val qual = name(imprt.qual).asInstanceOf[UseTree]
    TreeCopiers.copyImport(imprt)(qual = qual)
  }
}
