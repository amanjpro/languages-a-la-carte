package ch.usi.inf.l3.sana.ooj.types


import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.ooj
import tiny.symbols.Symbol
import tiny.names.Name
import tiny.types.Type
import calcj.types.PrimitiveType
import ooj.names.StdNames._

import scala.collection.immutable.Set


trait RefType extends Type {
  def name: Name
}


trait ClassTypeApi extends RefType {
  def qual: String
  def parents: Set[Symbol]

  def qualifiedName: String = s"$qual.${name.asString}"
  def allParents: Set[Type] = parents.flatMap { parent =>
    parent.tpe match {
      case Some(ctpe: ClassTypeApi)  => ctpe.allParents
      case _                         => Set.empty[Type]
    }
  }

  def =:=(t: Type): Boolean = t match {
    case ct: ClassTypeApi   =>
      lazy val res = this.allParents.foldLeft(true)((z, y) => {
        z && ct.allParents.exists(_ =:= y)
      })
      this.qualifiedName == ct.qualifiedName && res
    case _                  => false
  }

  def <:<(t: Type): Boolean = t match {
    // case ObjectType         => true
    case ct: ClassTypeApi   =>
      this.allParents.exists(_ =:= ct)
    case _                  => false
  }
}


// trait ArrayType extends RefType {
//   def elemType: Type
//
//   def =:=(t: Type): Boolean = t match {
//     case at: ArrayType      => elemType =:= at.elemType
//     case _                  => false
//   }
//   def =/=(t: Type): Boolean = !(this =:= t)
//   def <:<(t: Type): Boolean = t match {
//     case _: ObjectType      => true
//     case _                  => false
//   }
//
//   // FIXME: Follow Java's specification
//   def >:>(t: Type): Boolean = this =:= t
//
//   def name: Name   = ARRAY_TYPE_NAME
//   def show: String = name.asString
// }

// object StringType extends ClassType {
//   override def show: String = "String type"
//   def name: Name = Name("java.lang.String")
// }


case class ClassType(qual: String, name: Name, parents: Set[Symbol])
  extends ClassTypeApi

// object ObjectType extends ClassTypeApi {
//   val qual: String       = "java.lang"
//   val parents: Set[Type] = Set.empty
//
//   override def <:<(t: Type): Boolean = t match {
//     case ObjectType         => true
//     case _                  => false
//   }
//
//   override def >:>(t: Type): Boolean = t match {
//     case _: RefType         => true
//     case _                  => false
//   }
//
//   def name: Name = OBJECT_TYPE_NAME
// }

object NullType extends RefType {
  def =:=(t: Type): Boolean = this == t
  def <:<(t: Type): Boolean = t match {
    case _: PrimitiveType   => false
    case _                  => true
  }
  def name: Name = NULL_NAME
}

