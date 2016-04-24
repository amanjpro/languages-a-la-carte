package ch.usi.inf.l3.sana.calcj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj

import tiny.ast._
import calcj.ast._

import calcj.ast.operators._
import calcj.types._
import tiny.types._

trait TreeUtils {
  def narrowDown(lit: LiteralApi, tpe: Type): LiteralApi = {
    tpe match {
      case ShortType =>
        TreeCopiers.copyLiteral(lit)(constant = {
          ShortConstant(lit.constant.value.toString.toShort)
        })
      case CharType  =>
        TreeCopiers.copyLiteral(lit)(constant = {
          CharConstant(lit.constant.value.asInstanceOf[Int].toChar)
        })
      case ByteType  =>
        TreeCopiers.copyLiteral(lit)(constant = {
          ByteConstant(lit.constant.value.toString.toByte)
        })
      case _         =>
        lit
    }
  }


  def widen(lit: LiteralApi, tpe: Type): LiteralApi = {
    tpe match {
      case IntType         =>
        TreeCopiers.copyLiteral(lit)(constant = {
          lit.constant.value match {
            case ch: Char      => IntConstant(ch.toInt)
            case _             =>
              IntConstant(lit.constant.value.toString.toInt)
          }
        })
      case LongType        =>
        TreeCopiers.copyLiteral(lit)(constant = {
          lit.constant.value match {
            case ch: Char      => LongConstant(ch.toLong)
            case _             =>
              LongConstant(lit.constant.value.toString.toLong)
          }
        })
      case FloatType       =>
        TreeCopiers.copyLiteral(lit)(constant = {
          lit.constant.value match {
            case ch: Char      => FloatConstant(ch.toFloat)
            case _             =>
              FloatConstant(lit.constant.value.toString.toFloat)
          }
        })
      case DoubleType      =>
        TreeCopiers.copyLiteral(lit)(constant = {
          lit.constant.value match {
            case ch: Char      => DoubleConstant(ch.toDouble)
            case _             =>
              DoubleConstant(lit.constant.value.toString.toDouble)
          }
        })
      case _               =>
        lit
    }
  }
}
