package ch.usi.inf.l3.sana.brokenj.typechecker


import ch.usi.inf.l3.sana
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast._
import tiny.types._
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



@component
trait CaseTyperComponent extends TyperComponent {
  (cse: Case) => {
    val guards = cse.guards.map(typed(_).asInstanceOf[Expr])
    val body   = typed(cse.body)
    cse.copy(guards = guards, body = body)
  }
}



@component
trait SwitchTyperComponent extends TyperComponent {
  (switch: Switch) => {
    val expr   = typed(switch.expr).asInstanceOf[Expr]
    val cases  = switch.cases.map(cse => typed(cse).asInstanceOf[Case])
    cases.foreach { cse =>
      cse.guards.foreach { guard =>
        (expr.tpe, guard.tpe) match {
          case (Some(etpe), Some(gtpe)) if gtpe <:< etpe =>
            ()
          case (Some(etpe), Some(gtpe))                  =>
            error(TYPE_MISMATCH,
              gtpe.toString, etpe.toString, guard.pos, guard)
          case (_, Some(gtpe))                           =>
            ()
          case (Some(etpe), _)                           =>
            error(TYPE_MISMATCH,
              ErrorType.toString, etpe.toString, guard.pos, guard)
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
              expr.pos, expr)
    }
    switch.copy(expr = expr, cases = cases)
  }
}


@component
trait LabelTyperComponent extends TyperComponent {
  (label: Label) => {
    val stmt   = typed(label.stmt).asInstanceOf[Expr]
    label.copy(stmt =stmt)
  }
}

@component
trait ContinueTyperComponent extends TyperComponent {
  (continue: Continue) => continue
}

@component
trait BreakTyperComponent extends TyperComponent {
  (break: Break) => break
}
