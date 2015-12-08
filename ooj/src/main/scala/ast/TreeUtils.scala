package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import brokenj.ast
import tiny.ast.{Tree, NoTree, TypeUseApi}
import Implicits._
import ooj.symbols.SymbolUtils
import ooj.modifiers.Ops._
import ooj.ast.TreeExtractors._


trait TreeUtils extends ast.TreeUtils {
  def isConstructor(tree: Tree): Boolean =
    tree.symbol.map(SymbolUtils.isConstructor(_)) match {
      case Some(v)                                  => v
      case None                                     =>
        tree match {
          case mthd: MethodDefApi =>
            mthd.mods.isConstructor
          case _                  =>
            false
        }
    }

  def isType(tree: Tree): Boolean = tree match {
    case _: TypeUseApi                   => true
    case Select(_, _: TypeUseApi)        => true
    case _                               => false
  }
}

object TreeUtils extends TreeUtils

