package ch.usi.inf.l3.sana.arrayj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.brokenj

import tiny.symbols.Symbol
import tiny.types.Type
import tiny.ast.{Tree, Expr, UseTree}
import tiny.source.Position

trait TreeCopiers extends brokenj.ast.TreeCopiers {

  def copyArrayInitializer(template: ArrayInitializerApi)(
      elements: List[Expr] = template.elements): ArrayInitializerApi = {
    val res = TreeFactories.mkArrayInitializer(elements)
    copyProperties(template, res)
    res
  }


  def copyArrayAccess(template: ArrayAccessApi)(
    array: Expr = template.array,
    index: Expr = template.index): ArrayAccessApi = {
    val res = TreeFactories.mkArrayAccess(array, index)
    copyProperties(template, res)
    res
  }

  def copyArrayTypeUse(template: ArrayTypeUseApi)(
      tpt: UseTree = template.tpt): ArrayTypeUseApi = {
    val res = TreeFactories.mkArrayTypeUse(tpt)
    copyProperties(template, res)
    res
  }

  def copyArrayCreation(template: ArrayCreationApi)(
      array: Expr = template.array,
      size: Option[Expr] = template.size): ArrayCreationApi = {
    val res = TreeFactories.mkArrayCreation(array, size)
    copyProperties(template, res)
    res
  }
}


object TreeCopiers extends TreeCopiers
