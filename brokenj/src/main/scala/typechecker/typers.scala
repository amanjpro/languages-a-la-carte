/*
 * Copyright (c) <2015-2016>, see CONTRIBUTERS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package ch.usi.inf.l3.sana.brokenj.typechecker


import ch.usi.inf.l3.sana
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.types._
import primj.ast.Implicits._
import primj.ast.TreeExtractors._
import tiny.types.TypeUtils._
import tiny.symbols.{TypeSymbol, TermSymbol}
import tiny.source.Position
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.TyperComponent
import primj.typechecker.TypePromotions
import calcj.types._
import primj.symbols._
import brokenj.errors.ErrorCodes._
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
    val guards = cse.guards.map { cs =>
      val res = typed(cs).asInstanceOf[Expr]
      res match {
        case Literal(c)                                         =>
          res
        case guard                                              =>
          error(CASE_GUARD_NOT_CONSTANT_EXPRESSION,
              "", "", guard.pos)
          cs
      }
    }
    val body   = typed(cse.body)
    TreeCopiers.copyCase(cse)(guards = guards, body = body)
  }
}



@component
trait SwitchTyperComponent extends TyperComponent {
  (switch: SwitchApi) => {
    val expr   = typed(switch.expr).asInstanceOf[Expr]
    val cases  = switch.cases
      .map(cse => typed(cse).asInstanceOf[CaseApi])
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
    checkDistinctness(cases.flatMap(_.guards))
    TreeCopiers.copySwitch(switch)(expr = expr, cases = cases)
  }

  def checkDistinctness(l: List[Expr]): Unit = l match {
    case Nil                                =>
      ()
    case (Literal(c1)::tl)                  =>
      val notDistinct = tl.find { t => t match {
          case Literal(c2) =>
            c1 == c2
          case _           =>
            false
        }
      }
      notDistinct.foreach { guard =>
        error(NOT_DISTINCT_GUARD,
            "", "", guard.pos)
      }
      checkDistinctness(tl)
    case (_::tl)                            =>
      checkDistinctness(tl)
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
