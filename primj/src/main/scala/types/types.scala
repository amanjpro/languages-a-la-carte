package ch.usi.inf.l3.sana.primj.types


import ch.usi.inf.l3.sana
import sana.tiny.types._
import sana.tiny.types.TypeUtils._


trait MethodTypeApi extends Type {
  def ret: Type
  def params: List[Type]

  def =:=(t: Type): Boolean = t match {
    case that: MethodType =>
      val pcheck = checkList(params, that.params, _ =:= _)
      pcheck && this.ret =:= that.ret
    case _                => false
  }

  // TODO: In Java-like languages, method subtyping is not like that!!
  def <:<(t: Type): Boolean = {
    val isEquiv = this =:= t
    lazy val isSub = t match {
      case that: MethodType =>
        val pcheck = checkList(params, that.params, _ >:> _)
        pcheck && this.ret <:< that.ret
      case _                => false
    }
    isEquiv || isSub
  }

  def show: String = s"MethodType((${params.mkString(", ")}) => ${ret})"
}

case class MethodType(val ret: Type,
  val params: List[Type]) extends MethodTypeApi



case object VoidType extends Type {
  def =:=(other: Type): Boolean = this == other
  def <:<(other: Type): Boolean = this =:= other
}
