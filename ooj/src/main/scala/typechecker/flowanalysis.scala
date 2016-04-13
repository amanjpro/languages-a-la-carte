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
                  MethodDefApi => _,
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
  private var trueCaseSymbols: List[Symbol]   = Nil
  private var falseCaseSymbols: List[Symbol]  = Nil


  def add(sym: Symbol): Unit = {
    trueCaseSymbols = sym::trueCaseSymbols
    falseCaseSymbols = sym::falseCaseSymbols
  }


  def isDefinitelyAssigned(sym: Symbol): Boolean = {
    trueCaseSymbols.contains(sym) || falseCaseSymbols.contains(sym)
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

  def unify(env: FlowEnv): Unit = {
    trueCaseSymbols  = this.trueCaseSymbols.intersect(env.trueCaseSymbols)
    falseCaseSymbols = this.falseCaseSymbols.intersect(env.falseCaseSymbols)
  }
  def mask(lane: TrackCase): Unit = lane match {
    case TrueCase                  =>
      this.trueCaseSymbols = Nil
    case FalseCase                 =>
      this.falseCaseSymbols = Nil
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
    pkg.members.foreach( member => check((member, env)))
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
        check((member, env))
      case member: BlockApi                       =>
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
  (mthd: MethodDefApi) => {
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

    val eenv = env.duplicate
    eenv.unionTracks
    eenv.mask(TrueCase)


    check((tern.thenp, tenv))
    check((tern.elsep, eenv))

    env.mergeIn(tenv, eenv)
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
