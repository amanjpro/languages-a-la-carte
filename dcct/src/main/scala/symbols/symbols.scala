package ch.usi.inf.l3.sana.dcct.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.primj.types.VoidType
import sana.calcj.types.IntType
import sana.calcj.symbols.IntSymbol
import sana.tiny.symbols.{Symbol, TermSymbol, TypeSymbol}
import sana.tiny.modifiers.Flags
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.names.Name
import sana.tiny.names.StdNames.noname

trait IndexIntSymbol extends IntSymbol {
  override def name: Name = Name("Int")
}

object IndexIntSymbol extends IndexIntSymbol
