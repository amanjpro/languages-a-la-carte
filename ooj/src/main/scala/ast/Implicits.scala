package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana.tiny.ast.Tree

object Implicits {
  implicit class AugmentedTreeImpl(val tree: Tree) extends AugmentedTree
}

