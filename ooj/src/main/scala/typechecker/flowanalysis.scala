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

package ch.usi.inf.l3.sana.ooj.typechecker

import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.calcj
import sana.tiny


import ooj.ast._
import brokenj.ast._
import primj.ast.{ProgramApi => _,
                  MethodDefApi => PMethodDefApi,
                  _}
import tiny.ast._
import calcj.ast._
import calcj.ast.operators._
import calcj.types.BooleanType
import tiny.symbols.Symbol
import tiny.types.Type
import tiny.errors.ErrorReporting.{error,warning}
import ooj.modifiers.Ops._
import ooj.ast.Implicits._
import ooj.symbols.SymbolUtils
import ooj.ast.TreeExtractors._
import ooj.errors.ErrorCodes._

import tiny.dsl._
import tiny.core._


sealed trait TrackCase
case object TrueCase extends TrackCase
case object FalseCase extends TrackCase

class FlowEnv {
  private var trueCaseSymbols: Set[Symbol]   = Set.empty
  private var falseCaseSymbols: Set[Symbol]  = Set.empty

  def emptyTracks(): Unit = {
    this.falseCaseSymbols = Set.empty
    this.trueCaseSymbols  = Set.empty
  }

  def add(sym: Symbol): Unit = {
    trueCaseSymbols = trueCaseSymbols + sym
    falseCaseSymbols = falseCaseSymbols + sym
  }


  def isDefinitelyAssigned(sym: Symbol): Boolean = {
    trueCaseSymbols.exists(_ == sym) || falseCaseSymbols.exists(_ == sym)
  }

  def batchAdd(env: FlowEnv,
        lane: TrackCase): Unit = lane match {
    case TrueCase               =>
      trueCaseSymbols   = trueCaseSymbols.union(env.trueCaseSymbols)
    case FalseCase              =>
      falseCaseSymbols  = falseCaseSymbols.union(env.falseCaseSymbols)
  }

  def mergeIn(env1: FlowEnv,
              env2: FlowEnv): Unit = {

    val temp = new FlowEnv
    temp.batchAdd(env1, TrueCase)
    temp.batchAdd(env2, FalseCase)

    temp.intersectTracks
    batchAdd(temp, TrueCase)
    batchAdd(temp, FalseCase)
  }

  def union(env: FlowEnv): Unit = {
    trueCaseSymbols  = this.trueCaseSymbols.union(env.trueCaseSymbols)
    falseCaseSymbols = this.falseCaseSymbols.union(env.falseCaseSymbols)
  }
  def unify(env: FlowEnv): Unit = {
    trueCaseSymbols  = this.trueCaseSymbols.intersect(env.trueCaseSymbols)
    falseCaseSymbols = this.falseCaseSymbols.intersect(env.falseCaseSymbols)
  }
  def mask(lane: TrackCase): Unit = lane match {
    case TrueCase                  =>
      this.trueCaseSymbols = Set.empty
    case FalseCase                 =>
      this.falseCaseSymbols = Set.empty
  }


  def getTrack(lane: TrackCase): FlowEnv = lane match {
    case TrueCase                  =>
      val temp = duplicate
      temp.mask(FalseCase)
      temp
    case FalseCase                 =>
      val temp = duplicate
      temp.mask(TrueCase)
      temp
  }


  def unionTracks(): Unit = {
    trueCaseSymbols  = trueCaseSymbols.union(falseCaseSymbols)
    falseCaseSymbols = trueCaseSymbols
  }

  def intersectTracks(): Unit = {
    trueCaseSymbols  = trueCaseSymbols.intersect(falseCaseSymbols)
    falseCaseSymbols = trueCaseSymbols
  }

  def duplicate: FlowEnv = {
    val temp = new FlowEnv
    temp.trueCaseSymbols   = this.trueCaseSymbols
    temp.falseCaseSymbols  = this.falseCaseSymbols
    temp
  }
}

trait CompletenessStatus {
  def unify(other: CompletenessStatus): CompletenessStatus =
    (this, other) match {
      case (I, _)           => I
      case (_, I)           => I
      case (M, _)           => M
      case (_, M)           => M
      case (B, B)           => B
      case (R, B)           => B
      case (B, R)           => B
      case (C, B)           => B
      case (B, C)           => B
      case (_, B)           => M
      case (B, _)           => M
      case (N, _)           => N
      case (_, N)           => N
      case (C, _)           => C
      case (_, C)           => C
      case (R, _)           => R
      case (_, R)           => R
    }
}

trait AbruptStatus extends CompletenessStatus
case object I extends AbruptStatus                // infinite loop
case object R extends AbruptStatus                // for return
case object C extends AbruptStatus                // for continue
case object B extends AbruptStatus                // for break
case object M extends CompletenessStatus          // for uncertain breaks
case object N extends CompletenessStatus          // normal completion


/*
 * This phase (or these components) perform additional flow-analysis and check
 * the correctness of the program for:
 * <li> Definitive assignment -- Java Spec 1.0, Chapter: 16 - page: 383
 * <li> Unreachable statements -- Java Spec 1.0, Section: 14.19 - page: 295
 */
trait FlowCorrectnessCheckerComponent extends
  TransformationComponent[(Tree, FlowEnv), CompletenessStatus] {
  def check: ((Tree, FlowEnv)) => CompletenessStatus
}

@component(tree, env)
trait ProgramFlowCorrectnessCheckerComponent extends
        FlowCorrectnessCheckerComponent {
  (prg: ProgramApi) => {
    val z: CompletenessStatus = N
    prg.members.foreach( member => check((member, env)))
    N
  }
}

@component(tree, env)
trait CompilationUnitFlowCorrectnessCheckerComponent
  extends FlowCorrectnessCheckerComponent {
  (unit: CompilationUnitApi) => {
    check((unit.module, env))
  }
}

@component(tree, env)
trait PackageDefFlowCorrectnessCheckerComponent
  extends FlowCorrectnessCheckerComponent {
  (pkg: PackageDefApi) => {
    val z: CompletenessStatus = N
    pkg.members.foreach { member =>
      env.emptyTracks
      check((member, env))
    }
    N
  }
}

@component(tree, env)
trait ClassDefFlowCorrectnessCheckerComponent extends
    FlowCorrectnessCheckerComponent {
  (clazz: ClassDefApi) => {
    check((clazz.body, env))
  }
}

@component(tree, env)
trait TemplateFlowCorrectnessCheckerComponent extends
    FlowCorrectnessCheckerComponent {
  (template: TemplateApi) => {
    template.members.foreach {
      case member: MethodDefApi                   =>
        env.emptyTracks
        check((member, env))
      case member: BlockApi                       =>
        env.emptyTracks
        check((member, env))
      case _                                      =>
        ()
    }
    N
  }
}

@component(tree, env)
trait MethodDefFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (mthd: PMethodDefApi) => {
    check((mthd.body, env))
  }
}

@component(tree, env)
trait BlockFlowCorrectnessCheckerComponent extends
    FlowCorrectnessCheckerComponent {
  (block: BlockApi) => {
    val z = (N: CompletenessStatus, false)
    val (status, _) = block.stmts.foldLeft(z) {(z, stmt) =>
      z._1 match {
        case _: AbruptStatus if !z._2 =>
          error(UNREACHABLE_STATEMENT,
            "", "", stmt.pos)
          (z._1, true)
        case _: AbruptStatus          =>
          z
        case _                        =>
          val r = check((stmt, env))
          val s = if(r == B) B else r.unify(z._1)
          (s, z._2)
      }
    }
    status
  }
}

@component(tree, env)
trait ValDefFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (valdef: ValDefApi) => {
    check((valdef.rhs, env))
    valdef.rhs match {
      case NoTree   => ()
      case _        =>
        valdef.symbol.foreach(env.add(_))
    }
    N
  }
}


@component(tree, env)
trait IdentFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (id: IdentApi) => {
    id.symbol.foreach { sym =>
      if(sym.mods.isLocalVariable &&
          !env.isDefinitelyAssigned(sym))
        error(VARIABLE_MIGHT_NOT_HAVE_BEEN_INITIALIZED,
          "", "", id.pos)
    }
    N
  }
}

@component(tree, env)
trait AssignFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (assign: AssignApi) => {
    check((assign.rhs, env))
    assign.lhs.symbol.foreach(env.add(_))
    N
  }
}

// boring cases
@component(tree, env)
trait TernaryFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (tern: TernaryApi) => {
    check((tern.cond, env))
    val tenv = env.duplicate
    tenv.mask(FalseCase)
    tenv.unionTracks
    check((tern.thenp, tenv))

    val fenv = env.duplicate
    fenv.mask(TrueCase)
    fenv.unionTracks
    check((tern.elsep, fenv))

    env.mergeIn(tenv, fenv)
    N
  }
}
@component(tree, env)
trait IfFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (ifelse: IfApi) => {
    check((ifelse.cond, env))

    val tenv = env.duplicate
    tenv.mask(FalseCase)
    tenv.unionTracks
    val tstatus = check((ifelse.thenp, tenv))

    val eenv = env.duplicate
    eenv.mask(TrueCase)
    eenv.unionTracks
    val estatus = check((ifelse.elsep, eenv))


    // Now, neutralize them all together
    env.mergeIn(tenv, eenv)

    tstatus.unify(estatus)
  }
}

@component(tree, env)
trait CaseFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (cse: CaseApi) => {
    cse.guards.foreach( guard => check((guard, env)))
    check((cse.body, env)) match {
      case R              => R
      case _              => N
    }
  }
}

@component(tree, env)
trait SwitchFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (switch: SwitchApi) => {
    check((switch.expr, env))
    val z: CompletenessStatus = N
    switch.cases.foldLeft(z) {(z, cse) =>
      val r = check((cse, env))
      r.unify(z)
    }
  }
}

@component(tree, env)
trait WhileFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (wile: WhileApi) => {
    if(!wile.isDoWhile) {
      check((wile.cond, env))
      wile.cond match {
        case Literal(BooleanConstant(false)) =>
          error(UNREACHABLE_STATEMENT,
            "", "", wile.body.pos)
          N
        case _                               =>
          val benv = env.duplicate
          benv.mask(FalseCase)
          benv.unionTracks
          val r = check((wile.body, benv))
          wile.cond match {
            case Literal(BooleanConstant(true))  if r != B && r != M =>
              env.batchAdd(benv, TrueCase)
              env.unionTracks
              I
            case _                                                   =>
              env.batchAdd(benv, FalseCase)
              env.unionTracks
              if(r == R) R
              else N
          }
      }
    } else {
      val r = check((wile.body, env))
      if(r == C || r == N || r == M)
        check((wile.cond, env))
      env.mask(TrueCase)
      env.unionTracks
      wile.cond match {
        case Literal(BooleanConstant(true))  if r != B && r != M =>
          I
        case _                                                   =>
          if(r == R) R
          else N
      }
    }
  }
}

@component(tree, env)
trait ForFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (forloop: ForApi) => {
    forloop.inits.foreach( init => check((init, env)))
    check((forloop.cond, env))
    forloop.cond match {
      case Literal(BooleanConstant(false)) =>
        error(UNREACHABLE_STATEMENT,
          "", "", forloop.body.pos)
        N
      case _                               =>
        val benv = env.duplicate
        benv.mask(FalseCase)
        benv.unionTracks
        val r = check((forloop.body, benv))
        if(r == N || r == C || r == M) {
          forloop.steps.foreach( step => check((step, benv)))
        }
        forloop.cond match {
          case Literal(BooleanConstant(true)) if r != B && r != M =>
            env.batchAdd(benv, TrueCase)
            env.unionTracks
            I
          case _                                                  =>
            env.batchAdd(benv, FalseCase)
            env.unionTracks
            if(r == R) R
            else N
        }
    }
  }
}

@component(tree, env)
trait ApplyFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (apply: ApplyApi) => {
    val z: CompletenessStatus = N
    apply.args.foreach( arg => check((arg, env)))
    N
  }
}

@component(tree, env)
trait UnaryFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (unary: UnaryApi) => {
    check((unary.expr, env))
    unary.op match {
      case Not      if isBooleanType(unary.tpe)               =>
        env.intersectTracks
      case _                                                  =>
        ()
    }
    N
  }

  protected def isBooleanType(tpe: Option[Type]): Boolean =
    tpe.map(_ =:= BooleanType).getOrElse(false)
}

@component(tree, env)
trait BinaryFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (bin: BinaryApi) => {
    val lenv = env.duplicate
    check((bin.lhs, lenv))
    val renv = lenv.duplicate
    bin.op match {
      case And                                                     =>
        renv.mask(FalseCase)
        renv.unionTracks
        check((bin.rhs, renv))

        // Rule #1
        env.batchAdd(lenv, TrueCase)
        env.batchAdd(renv, TrueCase)

        // Rule #2
        env.batchAdd(lenv, FalseCase)
        env.batchAdd(lenv, FalseCase)
      case Or                                                      =>
        renv.mask(TrueCase)
        renv.unionTracks
        check((bin.rhs, renv))

        // Rule #1
        env.batchAdd(lenv, TrueCase)
        env.batchAdd(renv, TrueCase)

        // Rule #2
        env.batchAdd(lenv, FalseCase)
        env.batchAdd(renv, FalseCase)
      case BAnd | BOr | Eq | Neq | BXor if isBooleanType(bin.tpe)  =>
        check((bin.rhs, renv))

        // Rule #1
        env.batchAdd(lenv, TrueCase)
        env.batchAdd(renv, TrueCase)

        // Rule #2
        env.batchAdd(lenv, FalseCase)
        env.batchAdd(lenv, FalseCase)
      case _                                                       =>
        check((bin.rhs, lenv))
        env.batchAdd(lenv, TrueCase)
        env.batchAdd(lenv, FalseCase)
    }
    N
  }

  protected def isBooleanType(tpe: Option[Type]): Boolean =
    tpe.map(_ =:= BooleanType).getOrElse(false)
}

@component(tree, env)
trait CastFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (cast: CastApi) => {
    check((cast.expr, env))
  }
}



@component(tree, env)
trait ReturnFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (ret: ReturnApi) => {
    ret.expr.map(e => check((e, env)))
    R
  }
}

@component(tree, env)
trait NewFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (nw: NewApi) =>
    check((nw.app, env))
}

@component(tree, env)
trait SelectFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (slct: SelectApi) =>
    check((slct.tree, env))
}

@component(tree, env)
trait LabelFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (lbl: LabelApi) =>
    check((lbl.stmt, env)) match {
      case C            => C
      case _            => N
    }
}

@component(tree, env)
trait TypeUseFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (tuse: TypeUseApi) => N
}

@component(tree, env)
trait LiteralFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (lit: LiteralApi) => N
}

@component(tree, env)
trait BreakFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (brk: BreakApi) => B
}

@component(tree, env)
trait ContinueFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (cntnu: ContinueApi) => C
}

@component(tree, env)
trait ThisFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (ths: ThisApi) => N
}

@component(tree, env)
trait SuperFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (spr: SuperApi) => N
}
