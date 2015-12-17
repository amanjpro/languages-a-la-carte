package ch.usi.inf.l3.sana.calcj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.symbols.{TypeSymbol, Symbol}
import sana.tiny.types.Type
import sana.calcj.types._
import sana.tiny.modifiers.Flags
import sana.tiny.modifiers.Ops._
import sana.tiny.names.Name

case object IntSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(IntType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("int")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???



  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object CharSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(CharType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("char")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object ShortSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(ShortType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("short")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object ByteSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(ByteType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("byte")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object LongSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(LongType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("long")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object FloatSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(FloatType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("float")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}

case object DoubleSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(DoubleType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("double")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}


case object BooleanSymbol extends TypeSymbol {
  def tpe: Option[Type] = Some(BooleanType)
  def owner: Option[Symbol] = None
  def mods: Flags = noflags
  def name: Name = Name("boolean")

  def tpe_=(t: Option[Type]): Unit = ???
  def owner_=(t: Option[Symbol]): Unit = ???
  def mods_=(f: Flags): Unit = ???
  def name_=(name: Name) = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???
  override def defines(symbol: Symbol,
    p: Symbol => Boolean): Boolean = false
  override def getSymbol(name: Name,
    p: Symbol => Boolean): Option[Symbol] = None
}
