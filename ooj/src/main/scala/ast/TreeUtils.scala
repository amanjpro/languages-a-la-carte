package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import brokenj.ast
import tiny.ast.{Tree, NoTree}
import Implicits._
import ooj.symbols.SymbolUtils


trait TreeUtils extends ast.TreeUtils {
  def isConstructor(tree: Tree): Boolean =
    tree.symbol.map(SymbolUtils.isConstructor(_)).getOrElse(false)
}

object TreeUtils extends TreeUtils

