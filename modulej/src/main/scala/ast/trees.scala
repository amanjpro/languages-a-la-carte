package ch.usi.inf.l3.sana.modulej.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj
import sana.ppj

import tiny.ast.{Tree, UseTree}
import tiny.names.Name



/********************* AST Nodes *********************************/


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
