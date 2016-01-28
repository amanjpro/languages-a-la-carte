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

import sana.dsl._
import sana.core._


sealed trait SymbolCase
case object TrueCase extends SymbolCase
case object FalseCase extends SymbolCase

class DefinitiveAssignedEnv {
  private var trueCaseSymbols: List[Symbol]   = Nil
  private var falseCaseSymbols: List[Symbol]  = Nil


  def add(sym: Symbol): Unit = {
    trueCaseSymbols = sym::trueCaseSymbols
    falseCaseSymbols = sym::falseCaseSymbols
  }


  def isDefinitelyAssigned(sym: Symbol): Boolean = {
    trueCaseSymbols.contains(sym) || falseCaseSymbols.contains(sym)
  }

  def batchAdd(env: DefinitiveAssignedEnv,
        lane: SymbolCase): Unit = lane match {
    case TrueCase               =>
      trueCaseSymbols   = trueCaseSymbols.union(env.trueCaseSymbols)
    case FalseCase              =>
      falseCaseSymbols  = falseCaseSymbols.union(env.falseCaseSymbols)
  }

  def mergeIn(env1: DefinitiveAssignedEnv,
              env2: DefinitiveAssignedEnv): Unit = {

    val temp = new DefinitiveAssignedEnv
    temp.batchAdd(env1, TrueCase)
    temp.batchAdd(env2, FalseCase)

    temp.intersectTracks
    batchAdd(temp, TrueCase)
    batchAdd(temp, FalseCase)
  }

  def mask(lane: SymbolCase): Unit = lane match {
    case TrueCase                  =>
      this.trueCaseSymbols = Nil
    case FalseCase                 =>
      this.falseCaseSymbols = Nil
  }


  def getTrack(lane: SymbolCase): DefinitiveAssignedEnv = lane match {
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

  def duplicate: DefinitiveAssignedEnv = {
    val temp = new DefinitiveAssignedEnv
    temp.trueCaseSymbols   = this.trueCaseSymbols
    temp.falseCaseSymbols  = this.falseCaseSymbols
    temp
  }
}

// true:  means it completes abruptly
// false: means it completes normally
trait VariableDefinitionCheckerComponent
  extends TransformationComponent[(Tree, DefinitiveAssignedEnv), Boolean] {
  def check: ((Tree, DefinitiveAssignedEnv)) => Boolean
}

@component(tree, env)
trait ProgramVariableDefinitionCheckerComponent extends
        VariableDefinitionCheckerComponent {
  (prg: ProgramApi) => {
    prg.members.foldLeft(false)((z, member) => z || check((member, env)))
  }
}

@component(tree, env)
trait CompilationUnitVariableDefinitionCheckerComponent
  extends VariableDefinitionCheckerComponent {
  (unit: CompilationUnitApi) => {
    check((unit.module, env))
  }
}

@component(tree, env)
trait PackageDefVariableDefinitionCheckerComponent
  extends VariableDefinitionCheckerComponent {
  (pkg: PackageDefApi) => {
    pkg.members.foldLeft(false)((z, member) => z || check((member, env)))
  }
}

@component(tree, env)
trait ClassDefVariableDefinitionCheckerComponent extends
    VariableDefinitionCheckerComponent {
  (clazz: ClassDefApi) => {
    check((clazz.body, env))
  }
}

@component(tree, env)
trait TemplateVariableDefinitionCheckerComponent extends
    VariableDefinitionCheckerComponent {
  (template: TemplateApi) => {
    template.members.foreach {
      case member: MethodDefApi                   =>
        check((member, env))
      case member: BlockApi                       =>
        check((member, env))
      case _                                      =>
        ()
    }
    false
  }
}

@component(tree, env)
trait MethodDefVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (mthd: MethodDefApi) => {
    check((mthd.body, env))
  }
}

@component(tree, env)
trait BlockVariableDefinitionCheckerComponent extends
    VariableDefinitionCheckerComponent {
  (block: BlockApi) => {
    val r = block.stmts.foldLeft(false) {(z, stmt) =>
      if(!z)
        check((stmt, env))
      else z
    }
    // env.nutralize // WHAT IS THIS?
    r
  }
}

@component(tree, env)
trait ValDefVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (valdef: ValDefApi) => {
    check((valdef.rhs, env))
    valdef.rhs match {
      case NoTree   => ()
      case _        =>
        valdef.symbol.foreach(env.add(_))
    }
    false
  }
}


@component(tree, env)
trait IdentVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (id: IdentApi) => {
    id.symbol.foreach { sym =>
      if(sym.mods.isLocalVariable &&
          !env.isDefinitelyAssigned(sym))
        error(VARIABLE_MIGHT_NOT_HAVE_BEEN_INITIALIZED,
          "", "", id.pos)
    }
    false
  }
}

@component(tree, env)
trait AssignVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (assign: AssignApi) => {
    check((assign.rhs, env))
    assign.lhs.symbol.foreach(env.add(_))
    false
  }
}

// boring cases
@component(tree, env)
trait TernaryVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
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
    false
  }
}
@component(tree, env)
trait IfVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (ifelse: IfApi) => {
    check((ifelse.cond, env))

    val tenv = env.duplicate
    tenv.mask(FalseCase)
    tenv.unionTracks
    check((ifelse.thenp, tenv))

    val eenv = env.duplicate
    eenv.mask(TrueCase)
    eenv.unionTracks
    check((ifelse.elsep, eenv))


    // Now, neutralize them all together
    env.mergeIn(tenv, eenv)
    false
  }
}

@component(tree, env)
trait CaseVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (cse: CaseApi) => {
    cse.guards.foreach( guard => check((guard, env)))
    check((cse.body, env))
  }
}

@component(tree, env)
trait SwitchVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (switch: SwitchApi) => {
    check((switch.expr, env))
    switch.cases.foldLeft(false) {(z, cse) =>
      if(!z)
        check((cse, env))
      else z
    }
  }
}

@component(tree, env)
trait WhileVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (wile: WhileApi) => {
    if(!wile.isDoWhile) {
      check((wile.cond, env))
      wile.cond match {
        case Literal(BooleanConstant(true))  =>
          true
        case Literal(BooleanConstant(false)) =>
          false
        case _                               =>
          val benv = env.duplicate
          benv.mask(FalseCase)
          benv.unionTracks
          check((wile.body, benv))
          env.mask(TrueCase)
          env.unionTracks
          false
      }
    } else {
      check((wile.body, env))
      check((wile.cond, env))
      env.mask(TrueCase)
      env.unionTracks
      wile.cond match {
        case Literal(BooleanConstant(true))  =>
          true
        case _                               =>
          false
      }
    }
  }
}

@component(tree, env)
trait ForVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (forloop: ForApi) => {
    forloop.inits.foreach( init => check((init, env)))
    check((forloop.cond, env))
    forloop.cond match {
      case Literal(BooleanConstant(true))  =>
        true
      case Literal(BooleanConstant(false)) =>
        false
      case _                               =>
        val benv = env.duplicate
        benv.mask(FalseCase)
        benv.unionTracks
        check((forloop.body, benv))
        forloop.steps.foreach( step => check((step, benv)))
        env.mask(TrueCase)
        env.unionTracks
        false
    }
  }
}

@component(tree, env)
trait ApplyVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (apply: ApplyApi) => {
    apply.args.foldLeft(false)((z, arg) => z || check((arg, env)))
  }
}

@component(tree, env)
trait UnaryVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (unary: UnaryApi) => {
    check((unary.expr, env))
    unary.op match {
      case Not      if isBooleanType(unary.tpe)               =>
        env.intersectTracks
      case _                                                  =>
        ()
    }
    false
  }

  protected def isBooleanType(tpe: Option[Type]): Boolean =
    tpe.map(_ =:= BooleanType).getOrElse(false)
}

@component(tree, env)
trait BinaryVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
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
    false
  }

  protected def isBooleanType(tpe: Option[Type]): Boolean =
    tpe.map(_ =:= BooleanType).getOrElse(false)
}

@component(tree, env)
trait CastVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (cast: CastApi) => {
    check((cast.expr, env))
  }
}



@component(tree, env)
trait ReturnVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (ret: ReturnApi) => {
    ret.expr.map(e => check((e, env)))
    true
  }
}

@component(tree, env)
trait NewVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (nw: NewApi) =>
    check((nw.app, env))
}

@component(tree, env)
trait SelectVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (slct: SelectApi) =>
    check((slct.tree, env))
}

@component(tree, env)
trait LabelVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (lbl: LabelApi) =>
    check((lbl.stmt, env))
}

@component(tree, env)
trait TypeUseVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (tuse: TypeUseApi) => false
}

@component(tree, env)
trait LiteralVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (lit: LiteralApi) => false
}

@component(tree, env)
trait BreakVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (brk: BreakApi) => true
}

@component(tree, env)
trait ContinueVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (cntnu: ContinueApi) => true
}

@component(tree, env)
trait ThisVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (ths: ThisApi) => false
}

@component(tree, env)
trait SuperVariableDefinitionCheckerComponent extends
  VariableDefinitionCheckerComponent {
  (spr: SuperApi) => false
}
