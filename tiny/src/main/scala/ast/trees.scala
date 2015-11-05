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


trait TypeUseApi extends SimpleUseTree
trait IdentApi extends SimpleUseTree

case class TypeUse protected[ast](val name: Name) extends TypeUseApi

case class Ident protected[ast](val name: Name) extends IdentApi

case object NoTree extends Expr


case object ErrorTree extends Tree
