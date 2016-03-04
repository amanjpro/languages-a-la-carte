package ch.usi.inf.l3.sana.arrooj.types


import ch.usi.inf.l3.sana

import sana.tiny
import sana.ooj

import tiny.types.Type
import ooj.types.RefType
import tiny.names.Name
import tiny.symbols.Symbol

trait ArrayType extends RefType {
  def objectClassType: Type
  def componentType: Type

  def parents: Set[Symbol] = Set.empty
  def name: Name   = componentType match {
    case ct: RefType =>
      Name(s"[${ct.name.asString}]")
    case _           =>
      Name(s"[${componentType.toString}]")
  }


  def =:=(t: Type): Boolean = t match {
    case that: ArrayType    => this.componentType =:= that.componentType
    case _                  => false
  }


  def <:<(t: Type): Boolean = t match {
    case that: ArrayType    => this.componentType <:< that.componentType
    case _                  => t =:= objectClassType
  }

  override def toString: String = s"$componentType[]"
}

trait ArrayTypeExtractor {
  def unapply(tpe: ArrayType): Option[Type] = tpe match {
    case tpe: ArrayType      => Some(tpe.componentType)
    case null                => None
  }
}


object ArrayType extends ArrayTypeExtractor

class ArrayTypeImpl(val componentType: Type,
                      val objectClassType: Type) extends ArrayType
