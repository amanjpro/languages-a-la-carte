package ch.usi.inf.l3.sana.tiny.core


import ch.usi.inf.l3.sana.tiny.ast.Tree


trait CompilerInterface {
  def typeCheck(tree: Tree): Tree
  def load(fname: String): Option[Tree]
  def parse(source: String*): Tree
  def unparse(tree: Tree): String
}
