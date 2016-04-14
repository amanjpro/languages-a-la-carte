package ch.usi.inf.l3.sana.modulej.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj
import sana.ppj

import tiny.ast.{Tree, UseTree}
import tiny.names.Name
import ooj.ast.PackageDefApi



/********************* AST Nodes *********************************/

trait CompilationUnitApi extends ooj.ast.CompilationUnitApi {
  def imports: List[ImportApi]

  override def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = imports.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r2 = module.bottomUp(r1)(f)
    f(r2, this)
  }
}

trait ImportApi extends Tree {
  def qual: UseTree
  def isOnDemand: Boolean


  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r = f(z, qual)
    f(r, this)
  }
}


protected[ast] class Import(val qual: UseTree,
    val isOnDemand: Boolean) extends ImportApi {
  override def toString: String = s"Import($qual, $isOnDemand)"
}

protected[ast] class CompilationUnit(val imports: List[ImportApi],
  val module: PackageDefApi,
  val sourceName: String,
  val sourcePath: List[String]) extends CompilationUnitApi {
  override def toString: String =
    s"CompilationUnit($imports, $module, $sourceName, $sourcePath)"
}
