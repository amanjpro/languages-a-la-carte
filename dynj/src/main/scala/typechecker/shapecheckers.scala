package ch.usi.inf.l3.sana.dynj.typechecker


import ch.usi.inf.l3.sana
import sana.dynj
import sana.robustj
import sana.arrooj
import sana.ooj
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj


import tiny.dsl._
import tiny.ast.Tree
import calcj.ast.BinaryApi
import tiny.errors.ErrorReporting.{error,warning}
import primj.typechecker.ShapeCheckerComponent
import tiny.ast.Implicits._
import dynj.errors.ErrorCodes._
import dynj.ast.operators.InstanceOf
import robustj.ast.TreeUtils

@component
trait BinaryShapeCheckerComponent extends ShapeCheckerComponent {
  (bin: BinaryApi)     => {
    check(bin.lhs)
    check(bin.rhs)
    if(!isValidExpression(bin.lhs))
      error(BAD_EXPRESSION, "", "", bin.lhs.pos)

    bin.op match {
      case InstanceOf   if !isTypeUse(bin.rhs)         =>
        error(BAD_EXPRESSION, "", "", bin.rhs.pos)
      case _            if !isValidExpression(bin.rhs) =>
        error(BAD_EXPRESSION, "", "", bin.rhs.pos)
      case _                                           =>
        ()
    }
  }


  def isTypeUse(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)

  protected def isValidExpression(t: Tree): Boolean =
    TreeUtils.isValidExpression(t)
}
