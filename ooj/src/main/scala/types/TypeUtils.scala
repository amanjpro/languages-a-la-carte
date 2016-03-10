package ch.usi.inf.l3.sana.ooj.types

import ch.usi.inf.l3.sana
import sana.ooj.names.StdNames
import sana.calcj.types._
import sana.tiny.types.Type
import sana.tiny.ast.Expr
import sana.tiny.ast.Implicits._
import sana.ooj.symbols.SymbolUtils

trait TypeUtils extends sana.primj.types.TypeUtils {
  protected def javaLangPackageName: String = {
    val java = StdNames.JAVA_PACKAGE_NAME.asString
    val lang = StdNames.LANG_PACKAGE_NAME.asString
    s"$java.$lang"
  }
  lazy val objectClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.OBJECT_TYPE_NAME
    ClassType(qual, name, Set.empty)
  }

  lazy val stringClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.STRING_TYPE_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val booleanClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.BOOLEAN_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val characterClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.CHARACTER_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val integerClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.INTEGER_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val longClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.LONG_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val floatClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.FLOAT_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val doubleClassType: ClassTypeApi = {
    val qual            = javaLangPackageName
    val name            = StdNames.DOUBLE_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  override def unifyTernaryBranches(lhs: Expr, rhs: Expr): Option[Type] = {
    (lhs.tpe, rhs.tpe) match {
      case (Some(NullType), Some(tpe))                                 =>
        Some(tpe)
      case (Some(tpe), Some(NullType))                                 =>
        Some(tpe)
      case (Some(tpe1: NumericType),
            Some(tpe2: NumericType))                                   =>
        super.unifyTernaryBranches(lhs, rhs)
      case (Some(ltpe), Some(rtpe))                                    =>
        if(ltpe <:< rtpe)      Some(rtpe)
        else if(rtpe <:< ltpe) Some(ltpe)
        else                   None
      case _                                                           =>
        super.unifyTernaryBranches(lhs, rhs)
    }
  }

  def toBoxedType(tpe: Type): Option[Type] = tpe match {
    case BooleanType            =>
      Some(booleanClassType)
    case CharType               =>
      Some(characterClassType)
    case IntType                =>
      Some(integerClassType)
    case LongType               =>
      Some(longClassType)
    case FloatType              =>
      Some(floatClassType)
    case DoubleType             =>
      Some(doubleClassType)
    case _                      =>
      None
  }
}


object TypeUtils extends TypeUtils
