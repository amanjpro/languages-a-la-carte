package ch.usi.inf.l3.sana.tiny.ast

object Implicits {
  implicit class AugmentedTreeImpl(val tree: Tree) extends AugmentedTree
}

