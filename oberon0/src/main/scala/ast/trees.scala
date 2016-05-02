package ch.usi.inf.l3.sana.oberon0.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.arrayj

import tiny.ast.{Tree, DefTree, TypeTree, TermTree, UseTree}
import primj.ast.{BlockApi}
import tiny.names.Name

trait ModuleDefApi extends TermTree {
  def name: Name
  def declarations: List[DefTree]
  def block: Option[BlockApi]


  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = declarations.foldLeft(z)((z, y) => {
      y.bottomUp(z)(f)
    })
    val r2 = block.map(b => f(r1, b)).getOrElse(r1)
    f(r1, this)
  }
}


trait TypeDefApi extends TypeTree {
  def name: Name
  def tpt: UseTree

  def bottomUp[R](z: R)(f: (R, Tree) => R): R = {
    val r1 = f(z, tpt)
    f(r1, this)
  }
}




private[this] class ModuleDef(val name: Name,
  val declarations: List[DefTree],
  val block: Option[BlockApi]) extends ModuleDefApi {
  override def toString: String = s"ModuleDef($name, $declarations, $block)"
}

private[this] class TypeDef(val name: Name,
  val tpt: UseTree) extends TypeDefApi {
  override def toString: String = s"TypeDef($name, $tpt)"
}
