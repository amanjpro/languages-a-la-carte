package ch.usi.inf.l3.sana.brokenj.typechecker


import ch.usi.inf.l3.sana
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.types._
import primj.ast.Implicits._
import tiny.types.TypeUtils._
import tiny.symbols.{TypeSymbol, TermSymbol}
import tiny.source.Position
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.{TyperComponent, TypePromotions}
import calcj.types._
import primj.symbols._
import primj.errors.ErrorCodes._
import primj.types._
import primj.modifiers.Ops._
import brokenj.ast._

/*
Case: DONE
Swtich: DONE
Label: DONE
Break: DONE
Continue: DONE
*/



@component(tree, symbols)
trait CaseTyperComponent extends TyperComponent {
  (cse: CaseApi) => {
    val guards = cse.guards.map(cs => typed((cs, symbols)).asInstanceOf[Expr])
    val body   = typed((cse.body, symbols))
    TreeCopiers.copyCase(cse)(guards = guards, body = body)
  }
}



@component(tree, symbols)
trait SwitchTyperComponent extends TyperComponent {
  (switch: SwitchApi) => {
    val expr   = typed((switch.expr, symbols)).asInstanceOf[Expr]
    val cases  = switch.cases
      .map(cse => typed((cse, symbols)).asInstanceOf[CaseApi])
    cases.foreach { cse =>
      cse.guards.foreach { guard =>
        (expr.tpe, guard.tpe) match {
          case (Some(etpe), Some(gtpe)) if gtpe <:< etpe =>
            ()
          case (Some(etpe), Some(gtpe))                  =>
            error(TYPE_MISMATCH,
              gtpe.toString, etpe.toString, guard.pos)
          case (_, Some(gtpe))                           =>
            ()
          case (Some(etpe), _)                           =>
            error(TYPE_MISMATCH,
              ErrorType.toString, etpe.toString, guard.pos)
        }
      }
    }
    expr.tpe match {
      case Some(etpe) if(etpe =:= CharType ||
                         etpe =:= ByteType ||
                         etpe =:= ShortType ||
                         etpe =:= IntType)   =>
        ()
      case _                                 =>
        error(TYPE_MISMATCH,
              expr.tpe.getOrElse(ErrorType).toString,
              "char, byte, short or int",
              expr.pos)
    }
    TreeCopiers.copySwitch(switch)(expr = expr, cases = cases)
  }
}


@component(tree, symbols)
trait LabelTyperComponent extends TyperComponent {
  (label: LabelApi) => {
    val stmt   = typed((label.stmt, symbols)).asInstanceOf[Expr]
    TreeCopiers.copyLabel(label)(stmt = stmt)
  }
}

@component(tree, symbols)
trait ContinueTyperComponent extends TyperComponent {
  (continue: ContinueApi) => continue
}

@component(tree, symbols)
trait BreakTyperComponent extends TyperComponent {
  (break: BreakApi) => break
}
