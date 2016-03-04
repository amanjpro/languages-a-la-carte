package ch.usi.inf.l3.sana.arrooj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.tiny.modifiers.Flags
import sana.tiny.modifiers.Ops._
import sana.tiny.names.Name
import sana.tiny.symbols.{Symbol, TypeSymbol}
import sana.arrooj.types.{ArrayType, TypeUtils}
import sana.ooj.symbols.ClassSymbol



trait ArraySymbol extends ClassSymbol {
  var componentSymbol: Symbol
  def objectClassSymbol: ClassSymbol
  def members: List[Symbol]


  decls = decls ++ members

  def parents: List[ClassSymbol] = List(objectClassSymbol)
  def parents_=(parents: List[ClassSymbol]): Unit = ???

  override def declare(symbol: Symbol): Unit = ???
  override def delete(symbol: Symbol): Unit = ???

  def owner: Option[Symbol] = componentSymbol.owner
  def owner_=(onwer: Option[Symbol]): Unit = ???

  def name: Name = componentSymbol.name
  def name_=(name: Name): Unit = ???

  def mods: Flags = noflags
  def mods_=(mods: Flags): Unit = ???

  def tpe: Option[Type] = for {
    ctpe <- componentSymbol.tpe
    otpe <- objectClassSymbol.tpe
  } yield TypeUtils.mkArrayType(ctpe)
  def tpe_=(tpe: Option[Type]): Unit = ???
}


class ArraySymbolImpl(var componentSymbol: Symbol,
    val objectClassSymbol: ClassSymbol,
    val members: List[Symbol]) extends ArraySymbol
