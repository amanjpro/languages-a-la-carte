package ch.usi.inf.l3.sana.guod.ast.augmenters

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.symbols.Symbol
import sana.guod.ast.SynchronizedApi



trait AugmentedSynchronized {

  def tree: SynchronizedApi

  def identifierIndices: Option[(Int, Int)] =
    tree.attributes.get('identifierIndices).map(_.asInstanceOf[(Int, Int)])

  def identifierIndices_=(indices: (Int, Int)): Unit =
    tree.attributes = tree.attributes + ('identifierIndices -> indices)
}
