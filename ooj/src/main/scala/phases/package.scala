package ch.usi.inf.l3.sana.ooj

import ch.usi.inf.l3.sana.tiny
import tiny.symbols.Symbol
import tiny.ast.Tree

package object phases {
  type AssignerInput = (Tree, Option[Symbol])
}
