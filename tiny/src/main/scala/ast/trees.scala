package ch.usi.inf.l3.sana.tiny.ast

import ch.usi.inf.l3.sana
// import sana.core.SyntaxComponent
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.StdNames._
import sana.tiny.names.Name



trait Tree {
  var attributes: Attributes = noAttributes

  def bottomUp[R](z: R)(f: (R, Tree) => R): R
  def foreach(f: Tree => Unit): Unit = {
    bottomUp(())((z, y) => f(y))
  }
}




trait NamedTree extends Tree {
  def name: Name
}

trait DefTree extends NamedTree


trait TermTree extends DefTree

trait TypeTree extends DefTree

trait Expr extends Tree

trait UseTree extends Expr with NamedTree

trait SimpleUseTree extends UseTree


trait TypeUseApi extends SimpleUseTree {
  def bottomUp[R](z: R)(f: (R, Tree) => R): R = f(z, this)
}
trait IdentApi extends SimpleUseTree {
  def bottomUp[R](z: R)(f: (R, Tree) => R): R = f(z, this)
}

protected[ast] class TypeUse(val name: Name) extends TypeUseApi {
  override def toString: String = s"TypeUse($name)"
}

protected[ast] class Ident(val name: Name) extends IdentApi {
  override def toString: String = s"Ident($name)"
}

case object NoTree extends Expr {
  def bottomUp[R](z: R)(f: (R, Tree) => R): R = f(z, this)
}


case object ErrorTree extends Tree {
  def bottomUp[R](z: R)(f: (R, Tree) => R): R = f(z, this)
}
