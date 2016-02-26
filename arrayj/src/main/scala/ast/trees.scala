package ch.usi.inf.l3.sana.arrayj.ast

import ch.usi.inf.l3.sana
import sana.tiny

import tiny.ast.{Tree, Expr, UseTree}
import tiny.names.Name

/********************* AST Nodes *********************************/

trait ArrayInitializerApi extends Expr {
  def elements: List[Expr]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = elements.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    f(r1, this)
  }
}


trait ArrayAccessApi extends Expr {
  def array: Expr
  def index: Expr


  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = index.bottomUp(z)(f)
    f(r1, this)
  }
}

trait ArrayTypeUseApi extends UseTree {
  def tpt: UseTree
  def name: Name = tpt.name


  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    f(z, this)
  }
}


trait ArrayCreationApi extends Expr {
  def array: Expr
  def size: Option[Expr]

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, array)
    val r2 = size.map(f(r1, _)).getOrElse(r1)
    f(r2, this)
  }
}








protected[ast] class ArrayInitializer(val elements: List[Expr]) extends
  ArrayInitializerApi {

  override def toString: String = s"ArrayInitializer($elements)"
}

protected[ast] class ArrayAccess(val array: Expr,
  val index: Expr) extends ArrayAccessApi {

  override def toString: String = s"ArrayAccess($array, $index)"
}

protected[ast] class ArrayTypeUse(val tpt: UseTree) extends ArrayTypeUseApi {
  override def toString: String = s"ArrayTypeUse($tpt)"
}

protected[ast] class ArrayCreation(val array: Expr,
    val size: Option[Expr]) extends ArrayCreationApi {
  override def toString: String = s"ArrayCreation($array, $size)"
}
