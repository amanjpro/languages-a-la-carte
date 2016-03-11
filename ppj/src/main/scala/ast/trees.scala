package ch.usi.inf.l3.sana.ppj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj
import sana.ppj

import tiny.ast.{Tree, Expr}
import primj.ast.{BlockApi}



/********************* AST Nodes *********************************/


trait SynchronizedApi extends Expr {
  def expr:  Expr
  def block: BlockApi

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, expr)
    val r2 = f(r1, block)
    f(r2, this)
  }
}





protected[ast] class Synchronized(val expr: Expr,
    val block: BlockApi) extends SynchronizedApi {
  override def toString: String = s"Synchronized($expr, $block)"
}
