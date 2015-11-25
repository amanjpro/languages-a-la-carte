package ch.usi.inf.l3.sana.ooj.types

import ch.usi.inf.l3.sana
import sana.ooj.names.StdNames

trait TypeUtils {
  def objectClassType: ClassTypeApi = {
    val qual            = {
      val java = StdNames.JAVA_PACKAGE_NAME.asString
      val lang = StdNames.LANG_PACKAGE_NAME.asString
      s"$java.$lang"
    }
    val name            = StdNames.OBJECT_TYPE_NAME
    ClassType(qual, name, Set.empty)
  }
}


object TypeUtils extends TypeUtils
