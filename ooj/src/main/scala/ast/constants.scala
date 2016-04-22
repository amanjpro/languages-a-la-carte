package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.ooj
import sana.calcj



import tiny.types.Type
import ooj.types.{NullType, TypeUtils}
import calcj.ast.Constant


case object NullConstant extends Constant {
  type Value = Null

  def value: Null = ???

  val tpe: Type = NullType
}




case class StringConstant(value: String) extends Constant {
  type Value = String

  lazy val tpe: Type = TypeUtils.stringClassType
}




