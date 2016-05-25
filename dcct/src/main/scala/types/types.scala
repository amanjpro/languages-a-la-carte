package ch.usi.inf.l3.sana.dcct.types

import ch.usi.inf.l3.sana
import sana.tiny.types._
import sana.ooj.types._

trait CloudType extends Type {
  def =:=(other: Type): Boolean = this == other
  def <:<(other: Type): Boolean = {
     other match {
       case x:CloudType => true
       case _ => false
     }
   }
}

case object CIntType extends CloudType 

case object CStringType extends CloudType 

case object CSetType extends CloudType 

/*
 * I use the same string as in ooj 
 */
