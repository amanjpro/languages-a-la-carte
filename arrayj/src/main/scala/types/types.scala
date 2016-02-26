package ch.usi.inf.l3.sana.arrayj.types


import ch.usi.inf.l3.sana

import sana.tiny
import tiny.types.Type

trait ArrayTypeApi extends Type {
  def componentType: Type


  def =:=(t: Type): Boolean = t match {
    case that: ArrayTypeApi => this.componentType =:= that.componentType
    case _                  => false
  }


  def <:<(t: Type): Boolean = t match {
    case that: ArrayTypeApi => this.componentType <:< that.componentType
    case _                  => false
  }
}


case class ArrayType(val componentType: Type) extends ArrayTypeApi
