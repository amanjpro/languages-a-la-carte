package ch.usi.inf.l3.sana.dcct.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.primj.types.VoidType
import sana.calcj.types.IntType
import sana.dcct.types.{CIntType, CSetType}
import sana.calcj.symbols.IntSymbol
import sana.ooj.symbols.ClassSymbol
import sana.tiny.symbols.{Symbol, TermSymbol, TypeSymbol}
import sana.tiny.modifiers.Flags
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.names.Name
import sana.tiny.names.StdNames.noname

trait IndexIntSymbol extends IntSymbol {
  override def name: Name = Name("Int")
}
object IndexIntSymbol extends IndexIntSymbol

trait CloudIntSymbol extends IntSymbol {
  override def name: Name = Name("CInt")
}
object CloudIntSymbol extends CloudIntSymbol


// TODO maybe there is a better base class to extend.
// TODO I am not sure about this overriding
trait CloudStringSymbol extends TypeSymbol {
  override def tpe: Option[Type] = Some(CIntType)
  override def owner: Option[Symbol] = None
  override def mods: Flags = noflags
  override def name: Name = Name("CString")

  override def mods_=(f:Flags): Unit = ???
  override def name_=(x: Name): Unit = ???
  override def owner_=(x:Option[Symbol]): Unit = ???
  override def tpe_=(x: Option[Type]): Unit = ???
  
  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}
object CloudStringSymbol extends CloudStringSymbol

// Will desugar to a java set later on I guess
trait CloudSetSymbol extends ClassSymbol {
  override def name: Name = Name("CSet")
  override def parents = ???
  override def mods = ???
  override def owner = ???
  override def tpe = Some(CSetType)

  override def parents_=(x: List[ClassSymbol]): Unit = ???
  override def mods_=(f:Flags): Unit = ???
  override def name_=(x: Name): Unit = ???
  override def owner_=(x:Option[Symbol]): Unit = ???
  override def tpe_=(x: Option[Type]): Unit = ???

}
object CloudSetSymbol extends CloudSetSymbol

// TODO add a tuple type.

