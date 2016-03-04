package ch.usi.inf.l3.sana.arrooj.types

import ch.usi.inf.l3.sana
import sana.ooj.names.StdNames
import sana.calcj.types._
import sana.tiny.types.Type
import sana.tiny.ast.Expr
import sana.tiny.ast.Implicits._
import sana.ooj.symbols.SymbolUtils

trait TypeUtils extends sana.ooj.types.TypeUtils {
  def mkArrayType(componentType: Type): ArrayType =
    new ArrayTypeImpl(componentType, objectClassType)
}

object TypeUtils extends TypeUtils
