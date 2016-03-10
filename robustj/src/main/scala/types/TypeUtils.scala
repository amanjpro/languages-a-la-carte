package ch.usi.inf.l3.sana.robustj.types

import ch.usi.inf.l3.sana
import sana.calcj.types._
import sana.tiny.types.Type
import sana.tiny.ast.Expr
import sana.tiny.ast.Implicits._
import sana.ooj.types.{ClassTypeApi, ClassType}
import sana.robustj.names.StdNames

trait TypeUtils extends sana.arrooj.types.TypeUtils {
  lazy val throwableClassType: ClassTypeApi        = {
    val qual            = javaLangPackageName
    val name            = StdNames.THROWABLE_CLASS_NAME
    ClassType(qual, name, Set.empty)
  }
  lazy val errorClassType: ClassTypeApi            = {
    val qual            = javaLangPackageName
    val name            = StdNames.ERROR_CLASS_NAME
    ClassType(qual, name, Set.empty)
  }
  lazy val exceptionClassType: ClassTypeApi        = {
    val qual            = javaLangPackageName
    val name            = StdNames.EXCEPTION_CLASS_NAME
    ClassType(qual, name, Set.empty)
  }
  lazy val runtimeExceptionClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.RUNTIME_EXCEPTION_CLASS_NAME
    ClassType(qual, name, Set.empty)
  }


  def isCheckedException(tpe: Option[Type]): Boolean = tpe.map { tpe =>
    !(tpe <:< runtimeExceptionClassType) && tpe <:< exceptionClassType
  }.getOrElse(false)

  def isDefinitivelyCheckedException(p: Option[Type]): Boolean = p.map { tpe =>
    isCheckedException(p) && tpe =/= exceptionClassType
  }.getOrElse(false)
}

object TypeUtils extends TypeUtils
