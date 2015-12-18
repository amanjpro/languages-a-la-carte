package ch.usi.inf.l3.sana.ooj.types

import ch.usi.inf.l3.sana
import sana.ooj.names.StdNames
import sana.calcj.types._
import sana.tiny.types.Type
import sana.ooj.symbols.SymbolUtils

trait TypeUtils {
  lazy val objectClassType: ClassTypeApi = {
    val qual            = {
      val java = StdNames.JAVA_PACKAGE_NAME.asString
      val lang = StdNames.LANG_PACKAGE_NAME.asString
      s"$java.$lang"
    }
    val name            = StdNames.OBJECT_TYPE_NAME
    ClassType(qual, name, Set.empty)
  }

  lazy val stringClassType: ClassTypeApi = {
    val qual            = {
      val java = StdNames.JAVA_PACKAGE_NAME.asString
      val lang = StdNames.LANG_PACKAGE_NAME.asString
      s"$java.$lang"
    }
    val name            = StdNames.STRING_TYPE_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val booleanClassType: ClassTypeApi = {
    val qual            = {
      val java = StdNames.JAVA_PACKAGE_NAME.asString
      val lang = StdNames.LANG_PACKAGE_NAME.asString
      s"$java.$lang"
    }
    val name            = StdNames.BOOLEAN_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val characterClassType: ClassTypeApi = {
    val qual            = {
      val java = StdNames.JAVA_PACKAGE_NAME.asString
      val lang = StdNames.LANG_PACKAGE_NAME.asString
      s"$java.$lang"
    }
    val name            = StdNames.CHARACTER_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val integerClassType: ClassTypeApi = {
    val qual            = {
      val java = StdNames.JAVA_PACKAGE_NAME.asString
      val lang = StdNames.LANG_PACKAGE_NAME.asString
      s"$java.$lang"
    }
    val name            = StdNames.INTEGER_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val longClassType: ClassTypeApi = {
    val qual            = {
      val java = StdNames.JAVA_PACKAGE_NAME.asString
      val lang = StdNames.LANG_PACKAGE_NAME.asString
      s"$java.$lang"
    }
    val name            = StdNames.LONG_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val floatClassType: ClassTypeApi = {
    val qual            = {
      val java = StdNames.JAVA_PACKAGE_NAME.asString
      val lang = StdNames.LANG_PACKAGE_NAME.asString
      s"$java.$lang"
    }
    val name            = StdNames.FLOAT_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
  }

  lazy val doubleClassType: ClassTypeApi = {
    val qual            = {
      val java = StdNames.JAVA_PACKAGE_NAME.asString
      val lang = StdNames.LANG_PACKAGE_NAME.asString
      s"$java.$lang"
    }
    val name            = StdNames.DOUBLE_CLASS_NAME
    ClassType(qual, name, Set(SymbolUtils.objectClassSymbol))
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
