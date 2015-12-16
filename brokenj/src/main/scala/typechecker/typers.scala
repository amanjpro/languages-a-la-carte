package ch.usi.inf.l3.sana.brokenj.typechecker


import ch.usi.inf.l3.sana
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.types._
import tiny.ast.Implicits._
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
  (cse: CaseApi) => {
    val guards = cse.guards.map(typed(_).asInstanceOf[Expr])
    val body   = typed(cse.body)
    TreeCopiers.copyCase(cse)(guards = guards, body = body)
  }
}



@component
trait SwitchTyperComponent extends TyperComponent {
  (switch: SwitchApi) => {
    val expr   = typed(switch.expr).asInstanceOf[Expr]
    val cases  = switch.cases.map(cse => typed(cse).asInstanceOf[CaseApi])
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


@component
trait LabelTyperComponent extends TyperComponent {
  (label: LabelApi) => {
    val stmt   = typed(label.stmt).asInstanceOf[Expr]
    TreeCopiers.copyLabel(label)(stmt = stmt)
  }
}

@component
trait ContinueTyperComponent extends TyperComponent {
  (continue: ContinueApi) => continue
}

@component
trait BreakTyperComponent extends TyperComponent {
  (break: BreakApi) => break
}
