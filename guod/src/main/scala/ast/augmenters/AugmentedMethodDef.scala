package ch.usi.inf.l3.sana.guod.ast.augmenters

import ch.usi.inf.l3.sana


trait AugmentedMethodDef extends sana.ooj.ast.augmenters.AugmentedMethodDef {

  def locals: Int =
    tree.attributes.get('locals).map(_.asInstanceOf[Int]).getOrElse(Int.MaxValue)

  def locals_=(locals: Int): Unit =
    tree.attributes = tree.attributes + ('locals -> locals)
}
