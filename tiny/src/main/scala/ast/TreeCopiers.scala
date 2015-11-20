package ch.usi.inf.l3.sana.tiny.ast

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.source.Position
import sana.tiny.symbols.Symbol
import sana.tiny.names.Name
import Implicits._



trait TreeCopiers {

  protected def copyProperties(template: Tree,
      newTree: Tree): Unit = newTree.attributes = template.attributes

  def copyIdent(template: IdentApi)
            (name: Name): IdentApi = {
    val res = TreeFactories.mkIdent(name)
    copyProperties(template, res)
    res
  }

  def copyTypeUse(template: TypeUseApi)(name: Name): TypeUseApi = {
    val res = TreeFactories.mkTypeUse(name)
    copyProperties(template, res)
    res
  }
}


object TreeCopiers extends TreeCopiers
