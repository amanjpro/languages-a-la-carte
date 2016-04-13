package ch.usi.inf.l3.sana.dynj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.ooj
import sana.dynj
import sana.robustj


import tiny.dsl._

import tiny.errors.ErrorReporting.{error,warning}
import tiny.ast.Implicits._
import tiny.ast.{Tree, UseTree, Expr}
import tiny.types.{ErrorType, Type}
import calcj.ast.{BinaryApi, CastApi}
import calcj.types.{BooleanType, NumericType}
import ooj.types.RefType
import dynj.errors.ErrorCodes._
import dynj.ast.operators.InstanceOf
import robustj.ast.TreeCopiers
import calcj.typechecker.TyperComponent
import robustj.types.TypeUtils

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


@component
trait CastTyperComponent extends TyperComponent {
  (cast: CastApi)           => {
    val tpt  = typed(cast.tpt).asInstanceOf[UseTree]
    val expr = typed(cast.expr).asInstanceOf[Expr]
    val res: Option[Tree]  = for {
      ttpe <- tpt.tpe
      etpe <- expr.tpe
    } yield {
      if(ttpe <:< objectClassType &&
          etpe <:< objectClassType) {
        if(ttpe <:< etpe || etpe <:< ttpe) {
          tpt.tpe.foreach(cast.tpe = _)
          TreeCopiers.copyCast(cast)(tpt, expr)
        } else {
          error(TYPE_MISMATCH, "", "", cast.pos)
          val r = TreeCopiers.copyCast(cast)(tpt, expr)
          r.tpe = ErrorType
          r
        }
      } else if(ttpe.isInstanceOf[NumericType] &&
                etpe.isInstanceOf[NumericType]) {
        tpt.tpe.foreach(cast.tpe = _)
        TreeCopiers.copyCast(cast)(tpt, expr)
      } else if(ttpe <:< BooleanType &&
                etpe <:< BooleanType) {
        tpt.tpe.foreach(cast.tpe = _)
        TreeCopiers.copyCast(cast)(tpt, expr)
      } else {
        error(TYPE_MISMATCH, "", "", cast.pos)
        val r = TreeCopiers.copyCast(cast)(tpt, expr)
        r.tpe = ErrorType
        r
      }
    }
    res.getOrElse(cast)
  }


  protected def objectClassType: Type =
    TypeUtils.objectClassType

}

