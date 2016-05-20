package ch.usi.inf.l3.sana.tiny.modifiers

import scala.collection.immutable.Set

trait Flag {
  def |(flag: Flag): Flags = Flags(Set(this, flag))
  def |(flags: Flags): Flags = flags | this
}
case object COMPILED extends Flag




sealed class Flags(private val flags: Set[Flag]) {
  def |(flag: Flag): Flags   = Flags(flags + flag)
  def |(other: Flags): Flags = Flags(other.flags ++ flags)

  def hasAnyFlags: Boolean = flags != Nil
  def hasNoFlags: Boolean  = !hasAnyFlags

  def hasFlag(flag: Flag): Boolean = flags.contains(flag)

  def flagsAsSet: Set[Flag] = flags

  final def asString: String = flags.mkString(" | ")
  override def toString: String = asString
}
object Flags {
  def apply(flag: Flag): Flags = new Flags(Set(flag))
  def apply(flags: Set[Flag]): Flags = new Flags(flags)

  def unapplySeq(flags: Flags): Option[Set[Flag]] = flags match {
    case null                 => None
    case flags                => Some(flags.flags)
  }
}

object NoFlags extends Flags(Set.empty[Flag])
