package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.ooj
import sana.calcj



import tiny.types.Type
import ooj.types.NullType
import calcj.ast.Constant


case object NullConstant extends Constant {
  type Value = Null

  val value: Null = ???

  val tpe: Type = NullType
}




