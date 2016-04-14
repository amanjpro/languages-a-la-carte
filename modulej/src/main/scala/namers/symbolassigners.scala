package ch.usi.inf.l3.sana.modulej.namers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import sana.modulej

import tiny.dsl._
import primj.namers.SymbolAssignerComponent
import modulej.ast._
import ooj.ast.PackageDefApi
import modulej.symbols.CompilationUnitSymbol
import tiny.ast.Implicits._

@component
trait CompilationUnitSymbolAssignerComponent extends SymbolAssignerComponent {
  (cunit: CompilationUnitApi) => {
    val owner   = cunit.owner
    val sym     = CompilationUnitSymbol(Nil, None, cunit.sourceName,
                                  cunit.sourcePath, owner)
    owner.foreach(owner => {
      owner.declare(sym)
    })
    cunit.module.owner = sym
    cunit.imports.foreach(_.owner = sym)

    val pkg            = assign(cunit.module).asInstanceOf[PackageDefApi]
    val imports        = cunit.imports.map(assign(_).asInstanceOf[ImportApi])
    val importSymbols  = imports.flatMap(_.symbol)
    sym.module         = pkg.symbol
    sym.imports        = importSymbols
    TreeCopiers.copyCompilationUnit(cunit)(imports = imports, module = pkg)
  }
}
