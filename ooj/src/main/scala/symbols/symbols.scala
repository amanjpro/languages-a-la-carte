package ch.usi.inf.l3.sana.ooj.symbols

import ch.usi.inf.l3.sana
import sana.tiny.types.Type
import sana.primj.types.VoidType
import sana.tiny.symbols.{Symbol, TermSymbol, TypeSymbol}
import sana.tiny.modifiers.Flags
import sana.tiny.modifiers.Ops.noflags
import sana.tiny.names.Name
import sana.tiny.names.StdNames.noname





case class PackageSymbol(var name: Name,
        var owner: Option[Symbol]) extends TermSymbol {

  def mods: Flags = noflags
  def mods_=(mods: Flags): Unit = ???

  def tpe: Option[Type] = None
  def tpe_=(tpe: Option[Type]): Unit = ???

  def qualifiedName: String = {
    val ownersFullName = owner.flatMap {
      case psym: PackageSymbol => Some(psym.qualifiedName)
      case _                   => None
    }
    ownersFullName.map((oname) =>
        s"$oname.${name.asString}").getOrElse(name.asString)
  }

  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: PackageSymbol  =>
      this.qualifiedName == that.qualifiedName
    case _                    =>
      false
  }

  override def toString(): String = s"Package symbol: $name"
  override def hashCode(): Int = qualifiedName.hashCode * 43
}

case class ClassSymbol(var mods: Flags, var name: Name,
        var parents: List[ClassSymbol],
        var owner: Option[Symbol],
        var tpe: Option[Type]) extends TypeSymbol {

  override def equals(other: Any): Boolean = other match {
    case null                 => false
    case that: ClassSymbol    =>
      this.owner == that.owner &&
        this.parents == that.parents &&
        this.name == that.name
        this.tpe  == that.tpe
    case _                    =>
      false
  }

  override def toString(): String = s"Class symbol: $name"
  override def hashCode(): Int = name.hashCode * 43 + tpe.hashCode
}

case class CompilationUnitSymbol(var module: Option[Symbol],
  sourceName: String, sourcePath: List[String]) extends Symbol {

  def owner: Option[Symbol] = None
  def owner_=(tpe: Option[Symbol]): Unit = ???

  def mods: Flags = noflags
  def mods_=(tpe: Flags) = ???

  def name: Name = Name(sourceName)
  def name_=(nme: Name): Unit = ???

  def tpe: Option[Type] = None
  def tpe_=(tpe: Option[Type]): Unit = ???

  override def toString(): String = s"CompilationUnit symbol: $sourceName"
  override def hashCode(): Int = sourceName.hashCode * 43 + sourcePath.hashCode
}
