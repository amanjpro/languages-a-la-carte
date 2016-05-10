package ch.usi.inf.l3.sana.oberon0.types



import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj
import sana.oberon0



import tiny.types.Type
import tiny.names.Name
import ooj.types.RefType
import tiny.ast.Expr

trait RecordTypeApi extends RefType {
  def fields: Map[Name, Type]


  def name: Name = Name("<Record type>")
  def <:<(other: Type): Boolean = other match {
    case that: RecordTypeApi         =>
      fields.foldLeft(true)((z, y) => {
        val (n, t) = y
        val r = that.fields.get(n).map(t <:< _).getOrElse(false)
        z && r
      })
    case _                           =>
      false
  }

  def =:=(other: Type): Boolean = other match {
    case that: RecordTypeApi         =>
      if(this.fields.size == that.fields.size) {
        fields.foldLeft(true)((z, y) => {
          val (n, t) = y
          val r = that.fields.get(n).map(t =:= _).getOrElse(false)
          z && r
        })
      } else false
    case _                           =>
      false
  }
}



trait ArrayTypeApi extends RefType {
  def componentType: Type
  def size: Expr

  def name: Name = Name("<Array type>")
  def =:=(other: Type): Boolean = this == other
  def <:<(other: Type): Boolean = this =:= other
}


case class RecordType(val fields: Map[Name, Type]) extends RecordTypeApi
case class ArrayType(val componentType: Type,
  val size: Expr) extends ArrayTypeApi
