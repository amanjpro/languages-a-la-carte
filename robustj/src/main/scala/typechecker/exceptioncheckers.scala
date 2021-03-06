/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
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

package ch.usi.inf.l3.sana.robustj.typechecker

import ch.usi.inf.l3.sana
import sana.robustj
import sana.arrooj
import sana.arrayj
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.TransformationComponent
import tiny.dsl._

// ASTs
import robustj.ast._
import arrooj.ast.Implicits._
import arrayj.ast.{TreeUtils => _, TreeCopiers => _, TreeFactories => _, _}
import ooj.ast.{TreeUtils => _, TreeCopiers => _, TreeFactories => _,
                MethodDefApi => _, TreeUpgraders => _, _}
import brokenj.ast.{TreeUtils => _, TreeCopiers => _, TreeFactories => _, _}
import primj.ast.{TreeUtils => _, TreeCopiers => _, TreeFactories => _,
                MethodDefApi => PMethodDefApi, ProgramApi => _, _}
import calcj.ast.{TreeUtils => _, TreeCopiers => _, TreeFactories => _, _}
import tiny.ast.{TreeCopiers => _, TreeFactories => _, _}


import tiny.types.Type
import tiny.symbols.Symbol
import robustj.symbols.MethodSymbol
import tiny.errors.ErrorReporting.{error, warning}
import tiny.source.Position
import robustj.errors.ErrorCodes._
import robustj.types.TypeUtils


/*
Program: DONE
CompilationUnit: DONE
PackageDef: DONE
ClassDef: DONE
Template: DONE
ValDef: DONE
Block: DONE
New: DONE
Select: DONE
This: DONE
Super: DONE
Ident: DONE
TypeUse: DONE
Cast: DONE
Literal: DONE
Binary: DONE
Unary: DONE
Assign: DONE
If: DONE
While: DONE
For: DONE
Ternary: DONE
Return: DONE
Label: DONE
Break: DONE
Continue: DONE
Case: DONE
Switch: DONE
ArrayInitializer: DONE
ArrayAccess: DONE
ArrayTypeUse: DONE
ArrayCreation: DONE
Catch: DONE

MethodDef: DONE
Apply: DONE
Try:
Throw: DONE
*/



/**
 * A class to represent a handled (caught or declared) exception
 *
 * @param tpe the type of this exception
 * @param pos the position of this exception
 * @param state the state of this exception, possible states:
 *              <li> It is declared in the method signature (in the throws clause)
 *              <li> it is handled by a catch-block and is already thrown
 *              <li> it is handled by a catch-block and is not thrown yet
 */
case class HandledException(tpe: Type, pos: Position, state: State) {
  override def equals(other: Any): Boolean = other match {
    case null                     => false
    case that: HandledException   => this.pos == that.pos
    case _                        => false
  }

  override def hashCode: Int = 43 * this.pos.hashCode
}


/**
 * A utility trait for performing different exception-handling related checks
 */
trait ExceptionUtils {
  /**
   * Given a list of handled exceptions and an exception type, updates the first
   * handled exception that has the same type as `t`, if it has the state
   * UnusedCaught, it marks it as UsedCaught.
   *
   * @param t the type of exception to update
   * @param he the list of handled exceptions
   */
  def updateFirstOccurance(t: Type,
    he: List[HandledException]): Option[List[HandledException]] = he match {
      case (h::hs) if t <:< h.tpe          =>
        if(h.state == ThrownClause) Some(he)
        else Some(HandledException(h.tpe, h.pos, UsedCaught)::hs)
      case (h::hs) if t >:> h.tpe          =>
        if(h.state == ThrownClause)
          updateFirstOccurance(t, hs).map(h::_)
        else
          updateFirstOccurance(t, hs)
            .map(HandledException(h.tpe, h.pos, UsedCaught)::_)
      case (h::hs)                         =>
        updateFirstOccurance(t, hs).map(h::_)
      case Nil                             =>
        None
    }


  /**
   * Given two lists of handled-exceptions, unifies them. In case
   * the same exception has the state UsedCaught in one list, then in the
   * resulting list it will have UseCaught too.
   */
  def unify(fst: List[HandledException],
    snd: List[HandledException]): List[HandledException] = {
      for {
        f <- fst
      } yield {
        if(snd.exists(s =>
            (f == s && (f.state == UsedCaught || s.state == UsedCaught)))) {
          f.copy(state = UsedCaught)
        } else f
      }
  }

  /**
   * Given lists lists of handled-exceptions, unifies them. In case
   * the same exception has the state UsedCaught in one list, then in the
   * resulting list it will have UseCaught too.
   */
  def unify(fst: List[HandledException],
    snd: List[HandledException],
    thrd: List[HandledException]): List[HandledException] = for {
    f <- fst
  } yield {
    if(snd.exists(s =>
        (f == s && (f.state == UsedCaught || s.state == UsedCaught))) ||
        thrd.exists(s =>
        (f == s && (f.state == UsedCaught || s.state == UsedCaught)))) {
      f.copy(state = UsedCaught)
    } else f
  }
}

object ExceptionUtils extends ExceptionUtils

/** The state of an exception */
trait State
/** An exception is declared in the method signature (throws clause) */
case object ThrownClause extends State
/** An exception is caught by a catch block, and is already thrown */
case object UsedCaught extends State
/** An exception is caught by a catch block, and is not thrown yet */
case object UnusedCaught extends State

/**
 * This phase makes that all reported "checked" exceptions are declared, and
 * all declared exceptions are declared.
 */
trait ExceptionHandlingCheckerComponent extends
  TransformationComponent[(Tree, List[HandledException]),
                                        List[HandledException]] {
  def check: ((Tree, List[HandledException])) => List[HandledException]
}



// Interesting cases
@component(tree, handledExceptions)
trait MethodDefExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (mthd: PMethodDefApi) => {
    mthd match {
      case mthd: MethodDefApi       =>
        val he1 = getHandledExceptions(mthd)
        val he2 = check((mthd.body, he1 ++ handledExceptions))
        val he3 = he2.drop(he1.length)
        unify(he3, handledExceptions)
      case mthd: PMethodDefApi            =>
        val res = TreeUpgraders.upgradeMethodDef(mthd)
        check((res, handledExceptions))
    }
  }


  /** @see {{{ExceptionUtils.unify}}} */
  protected def unify(fst: List[HandledException],
                      snd: List[HandledException]): List[HandledException] =
    ExceptionUtils.unify(fst, snd)

  /**
   * Return all the declared (handled) exception of a method
   *
   * @param mthd the method to return its declared (handled) exceptions
   */
  protected def getHandledExceptions(
        mthd: MethodDefApi): List[HandledException] = {
    mthd.symbol.map {
      case sym: MethodSymbol =>
        for {
          sym  <- sym.throwsSymbols
          tpe  <- sym.tpe
          pos  <- mthd.pos
        } yield HandledException(tpe, pos, ThrownClause)
      case e                 =>
        Nil
    }.getOrElse(Nil)
  }

}

@component(tree, handledExceptions)
trait ThrowExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {

  (thrw: ThrowApi) => {
    val he = check((thrw.expr, handledExceptions))
    thrw.expr.tpe.flatMap(updateFirstOccurance(_, he)).getOrElse(he)
  }

  /** @see [[ExceptionUtils.updateFirstOccurance]] */
  protected def updateFirstOccurance(t: Type,
    he: List[HandledException]): Option[List[HandledException]] =
      ExceptionUtils.updateFirstOccurance(t, he)
}
@component(tree, handledExceptions)
trait ApplyExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (apply: ApplyApi) => {
    val he = check((apply.fun, handledExceptions))
    val he1 = checkThrownExceptions(apply, he)
    apply.args.foldLeft(he1) { (z, y) =>
      check((y, z))
    }
  }


  /** @see [[ExceptionUtils.updateFirstOccurance]] */
  protected def updateFirstOccurance(t: Type,
    he: List[HandledException]): Option[List[HandledException]] =
      ExceptionUtils.updateFirstOccurance(t, he)

  /**
   * Check if all the possible exceptions that may be thrown by this
   * method/function is handled either by a try-catch statement or
   * declared by the enclosing method of this expression.
   *
   * @param apply the method/function application in question
   * @param he the exceptions that are handled at this point
   */
  protected def checkThrownExceptions(apply: ApplyApi,
    he: List[HandledException]): List[HandledException] = {
    val thrownExceptions = apply.fun.symbol match {
      case Some(s: MethodSymbol)       => s.throwsSymbols
      case _                           => Nil
    }

    thrownExceptions.foldLeft(he) { (z, y) =>
      if(isCheckedException(y.tpe)) {
        y.tpe.flatMap(updateFirstOccurance(_, z)) match {
          case Some(he)               => he
          case None                   =>
            error(UNREPORTED_EXCEPTION, "", "", apply.pos)
            z
        }
      } else z
    }
  }

  /** @see [[robustj.types.TypeUtils.isCheckedException]] */
  protected def isCheckedException(tpe: Option[Type]): Boolean =
    TypeUtils.isCheckedException(tpe)
}

@component(tree, handledExceptions)
trait TryExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (tri: TryApi) => {
    val handled = getHandledExceptions(tri.catches)
    val he1 = handled ++ handledExceptions
    val he2 = check((tri.tryClause, he1))
    val (handled2, he3) = he2.splitAt(handled.size)
    val he4 = tri.catches.foldLeft(he3)((z, y) => {
      val z2 = check((y, z))
      unify(z, z2)
    })
    val he5 = tri.finallyClause.map(f => check((f, he4))).getOrElse(he4)
    checkHandledExceptions(handled2)
    unify(he3, he4, he5)
  }

  /** @see {{{ExceptionUtils.unify}}} */
  protected def unify(fst: List[HandledException],
                      snd: List[HandledException]): List[HandledException] =
    ExceptionUtils.unify(fst, snd)

  /** @see {{{ExceptionUtils.unify}}} */
  protected def unify(fst: List[HandledException],
                      snd: List[HandledException],
                      thd: List[HandledException]): List[HandledException] =
    ExceptionUtils.unify(fst, snd, thd)

  /**
   * Return all the caught (handled) exceptions by this try-catch statement
   *
   * @param catches the list of catch-blocks of this try-catch statement
   */
  protected def getHandledExceptions(catches: List[CatchApi]):
    List[HandledException] = for {
        ctch <- catches
        tpe  <- ctch.eparam.tpe
        pos  <- ctch.eparam.pos
      } yield HandledException(tpe, pos, UnusedCaught)

  /**
   * Checks if all the definitively checked exceptions that are handled
   * are actually thrown in the body of the method
   *
   * @param he the list to be checked
   */
  protected def checkHandledExceptions(he: List[HandledException]): Unit =
    for {
      handled <- he if isDefinitivelyCheckedException(Some(handled.tpe)) &&
                       handled.state == UnusedCaught
    } {
      error(HANDLING_NON_THROWN_EXCEPTION, "", "", Some(handled.pos))
    }

  /** @see [[robustj.types.TypeUtils.isDefinitivelyCheckedException]] */
  protected def isDefinitivelyCheckedException(p: Option[Type]): Boolean =
    TypeUtils.isDefinitivelyCheckedException(p)
}

// boring cases




@component(tree, handledExceptions)
trait ProgramExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (prg: ProgramApi)  => {
    prg.members.foreach(m => check((m, Nil)))
    Nil
  }
}

@component(tree, handledExceptions)
trait CompilationUnitExceptionHandlingCheckerComponent extends
    ExceptionHandlingCheckerComponent {
  (cunit: CompilationUnitApi)  => {
    check((cunit.module, Nil))
  }
}

@component(tree, handledExceptions)
trait PackageDefExceptionHandlingCheckerComponent extends
  ExceptionHandlingCheckerComponent {
  (pkg: PackageDefApi)  => {
    pkg.members.foreach(m => check((m, Nil)))
    Nil
  }
}

@component(tree, handledExceptions)
trait ClassDefExceptionHandlingCheckerComponent extends
  ExceptionHandlingCheckerComponent {
  (clazz: ClassDefApi)  => {
    check((clazz.body, Nil))
  }
}

@component(tree, handledExceptions)
trait TemplateExceptionHandlingCheckerComponent extends
  ExceptionHandlingCheckerComponent {
  (template: TemplateApi)  => {
    template.members.foreach(m => check((m, Nil)))
    Nil
  }
}

@component(tree, handledExceptions)
trait NewExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (nw: NewApi)  => {
    check((nw.app, handledExceptions))
  }
}

@component(tree, handledExceptions)
trait SelectExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (select: SelectApi)  => {
    val he = check((select.qual, handledExceptions))
    check((select.tree, he))
  }
}

@component(tree, handledExceptions)
trait ThisExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (ths: ThisApi)  => handledExceptions
}

@component(tree, handledExceptions)
trait SuperExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (spr: SuperApi)  => handledExceptions
}


@component(tree, handledExceptions)
trait ArrayCreationExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (creation: ArrayCreationApi) => {
    creation.size.map(size => check((size, handledExceptions)))
      .getOrElse(handledExceptions)
  }
}

@component(tree, handledExceptions)
trait ArrayAccessExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (access: ArrayAccessApi) => {
    val he = check((access.array, handledExceptions))
    check((access.index, he))
  }
}

// @component(tree, handledExceptions)
// trait ArrayTypeUseExceptionHandlingCheckerComponent
//   extends ExceptionHandlingCheckerComponent {
//   (tuse: ArrayTypeUseApi) => handledExceptions
// }

@component(tree, handledExceptions)
trait ArrayInitializerExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (init: ArrayInitializerApi) => {
    init.elements.foldLeft(handledExceptions) { (z, y) =>
      check((y, z))
    }
  }
}
// @component(tree, handledExceptions)
// trait ContinueExceptionHandlingCheckerComponent
//   extends ExceptionHandlingCheckerComponent {
//   (cont: ContinueApi) => handledExceptions
// }
//
// @component(tree, handledExceptions)
// trait BreakExceptionHandlingCheckerComponent
//   extends ExceptionHandlingCheckerComponent {
//   (break: BreakApi) => handledExceptions
//
// }


@component(tree, handledExceptions)
trait LabelExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (label: LabelApi) => check((label.stmt, handledExceptions))
}

@component(tree, handledExceptions)
trait CatchExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (ctch: CatchApi) => check((ctch.catchClause, handledExceptions))
}

// boring cases

@component(tree, handledExceptions)
trait ValDefExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (vdef: ValDefApi) =>
    check((vdef.rhs, handledExceptions))
}

@component(tree, handledExceptions)
trait AssignExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (assgn: AssignApi) => {
    val he = check((assgn.lhs, handledExceptions))
    check((assgn.rhs, he))
  }
}

@component(tree, handledExceptions)
trait ReturnExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (ret: ReturnApi) =>
    ret.expr.map(expr => check((expr, handledExceptions)))
      .getOrElse(handledExceptions)
}

@component(tree, handledExceptions)
trait IfExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (ifelse: IfApi) => {
    val he1 = check((ifelse.cond, handledExceptions))
    val he2 = check((ifelse.thenp, he1))
    check((ifelse.elsep, he2))
  }
}

@component(tree, handledExceptions)
trait WhileExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (wile: WhileApi) => {
    val he = check((wile.cond, handledExceptions))
    check((wile.body, he))
  }
}

@component(tree, handledExceptions)
trait BlockExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (block: BlockApi) => {
    block.stmts.foldLeft(handledExceptions){(z, y) =>
      check((y, z))
    }
  }
}

@component(tree, handledExceptions)
trait ForExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (forloop: ForApi) => {
    val he1 = forloop.inits.foldLeft(handledExceptions){(z, y) =>
      check((y, z))
    }
    val he2 = check((forloop.cond, he1))
    val he3 = forloop.steps.foldLeft(he2){(z, y) =>
      check((y, z))
    }
    check((forloop.body, he3))
  }
}

@component(tree, handledExceptions)
trait TernaryExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (ternary: TernaryApi) => {
    val he1 = check((ternary.cond, handledExceptions))
    val he2 = check((ternary.thenp, he1))
    check((ternary.elsep, he2))
  }
}



@component(tree, handledExceptions)
trait CastExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (cast: CastApi) => {
    check((cast.expr, handledExceptions))
  }
}

@component(tree, handledExceptions)
trait UnaryExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (unary: UnaryApi) => {
    check((unary.expr, handledExceptions))
  }
}

@component(tree, handledExceptions)
trait BinaryExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (binary: BinaryApi) => {
    val he = check((binary.lhs, handledExceptions))
    check((binary.rhs, he))
  }
}


@component(tree, handledExceptions)
trait SwitchExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (switch: SwitchApi) => {
    val he = check((switch.expr, handledExceptions))
    switch.cases.foldLeft(he){(z, y) =>
      check((y, z))
    }
  }
}


@component(tree, handledExceptions)
trait CaseExceptionHandlingCheckerComponent
  extends ExceptionHandlingCheckerComponent {
  (cse: CaseApi) => {
    check((cse.body, handledExceptions))
  }
}

// even more boring cases
// @component(tree, handledExceptions)
// trait IdentExceptionHandlingCheckerComponent
//   extends ExceptionHandlingCheckerComponent {
//   (ident: IdentApi) => handledExceptions
// }
//
// @component(tree, handledExceptions)
// trait TypeUseExceptionHandlingCheckerComponent
//   extends ExceptionHandlingCheckerComponent {
//   (tuse: TypeUseApi) => handledExceptions
// }
//
//
//
// @component(tree, handledExceptions)
// trait LiteralExceptionHandlingCheckerComponent
//   extends ExceptionHandlingCheckerComponent {
//   (lit: LiteralApi) => handledExceptions
// }
