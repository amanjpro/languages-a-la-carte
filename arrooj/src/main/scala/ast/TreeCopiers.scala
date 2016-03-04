package ch.usi.inf.l3.sana.arrooj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.brokenj
import sana.ooj
import sana.arrayj

import tiny.symbols.Symbol
import tiny.types.Type
import tiny.ast.{Tree, Expr, UseTree}
import tiny.source.Position
import arrayj.ast.{TreeCopiers => ATreeCopiers, _}

trait TreeCopiers extends ooj.ast.TreeCopiers {
  def copyArrayInitizalizer(template: ArrayInitializerApi)(
      elements: List[Expr] = template.elements): ArrayInitializerApi =
    ATreeCopiers.copyArrayInitizalizer(template)(elements)


  def copyArrayAccess(template: ArrayAccessApi)(
    array: Expr = template.array,
    index: Expr = template.index): ArrayAccessApi =
    ATreeCopiers.copyArrayAccess(template)(array, index)

  def copyArrayTypeUse(template: ArrayTypeUseApi)(
      tpt: UseTree = template.tpt): ArrayTypeUseApi =
    ATreeCopiers.copyArrayTypeUse(template)(tpt)

  def copyArrayCreation(template: ArrayCreationApi)(
      array: Expr = template.array,
      size: Option[Expr] = template.size): ArrayCreationApi =
    ATreeCopiers.copyArrayCreation(template)(array, size)
}

object TreeCopiers extends TreeCopiers

