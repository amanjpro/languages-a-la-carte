package ch.usi.inf.l3.sana.dynj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.ooj
import sana.dynj


import sana.dsl._

import tiny.errors.ErrorReporting.{error,warning}
import tiny.ast.Implicits._
import tiny.types.Type
import calcj.ast.BinaryApi
import calcj.types.BooleanType
import ooj.types.RefType
import dynj.errors.ErrorCodes._
import dynj.ast.operators.InstanceOf


@component
trait BinaryTyperComponent extends ooj.typechecker.BinaryTyperComponent {

  override protected def binaryTyper(ltpe: Type,
    rtpe: Type, bin: BinaryApi): Option[(Type, Type, Type)] = bin.op match {
    case InstanceOf                                 =>
      if(ltpe.isInstanceOf[RefType] && rtpe.isInstanceOf[RefType]) {
        Some((ltpe, rtpe, BooleanType))
      } else {
        error(DYNAMIC_TYPE_CHECK_NONE_REF_TYPE, "", "", bin.pos)
        None
      }
    case _                                          =>
      super.binaryTyper(ltpe, rtpe, bin)
  }

}

