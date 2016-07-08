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

package ch.usi.inf.l3.sana.guod.codegen



import ch.usi.inf.l3.sana
import sana.guod
import sana.tiny
import sana.ooj
import sana.calcj



import tiny.dsl._
import tiny.core.TransformationComponent
import tiny.modifiers.Flags
import guod.modifiers.ModifiersUtils
import guod.ast.TreeFactories
import guod.ast.TreeExtractors._
import guod.ast._
import tiny.symbols.Symbol
import calcj.types.{LongType, DoubleType}
import tiny.names.StdNames.noname
import guod.symbols.SymbolUtils
import guod.ast.Implicits._
import ooj.modifiers.Ops._

class Env {
  var env: Map[Symbol, Int] = Map.empty
  protected var current: Int          = -1
  protected var max: Int              = -1

  protected def advanceTwice(): Unit  = {
    current = current + 2
    this.max = max.max(current)
  }

  def advanceOnce(): Unit             = {
    current = current + 1
    this.max = max.max(current)
  }


  def newMax(max: Int): Unit = this.max = current.max(max)
  def getMax(): Int          = this.max.max(current)

  def newCurrentIndex(current: Int): Unit   = {
    this.current   = current
  }

  def currentIndex: Int  = current

  def add(symbol: Symbol): Unit = {
    val index = current
    symbol.tpe match {
      case Some(tpe)    if tpe =:= LongType || tpe =:= DoubleType =>
        advanceTwice()
      case _                                                      =>
        advanceOnce()
    }
    env = env + (symbol -> index)
  }


  def getIndex(symbol: Symbol): Option[Int] =
    env.get(symbol)

  def newMethod(symbol: Symbol): Unit = {
    env  = Map.empty
    current = if(symbol.mods.isStatic) 0 else 1
    max     = current
  }


  def duplicate: Env = {
    val temp            = new Env
    temp.env            = this.env
    temp.current        = this.current
    temp.max            = this.max
    temp
  }
}

trait LocalVariablesComponent extends
  TransformationComponent[(Tree, Env), Unit] {
  def subst: ((Tree, Env)) => Unit
}

// Ident: DONE
// TypeUse: DONE
// Cast: DONE
// Literal: DONE
// Unary: DONE
// Binary: DONE
// Block: DONE
// While: DONE
// If: DONE
// For: DONE
// Assign: DONE
// Apply: DONE
// ValDef: DONE
// Ternary: DONE
// Return: DONE
// Label: DONE
// Break: DONE
// Continue: DONE
// Case: DONE
// Switch: DONE
// ArrayInitializer: DONE
// ArrayTypeUse: DONE
// ArrayAccess: DONE
// ArrayCreation: DONE
// Program: DONE
// PackageDef: DONE
// ClassDef: DONE
// This: DONE
// Super: DONE
// Template: DONE
// New: DONE
// Select: DONE
// Try: DONE
// MethodDef: DONE
// Catch: DONE
// Throw: DONE
// Synchronized: DONE
// CompilationUnit: DONE


@component(tree, env)
trait ProgramLocalVariablesComponent extends LocalVariablesComponent {
  (program: ProgramApi) => {
    program.members.foreach(m => subst((m, env)))
  }
}

@component(tree, env)
trait PackageDefLocalVariablesComponent extends LocalVariablesComponent {
  (pkg: PackageDefApi) => {
    pkg.members.foreach(m => subst((m, env)))
  }
}


@component(tree, env)
trait TemplateLocalVariablesComponent extends LocalVariablesComponent {
  (template: TemplateApi) => {
    template.members.foreach(m => subst((m, env)))
  }
}


@component(tree, env)
trait CompilationUnitLocalVariablesComponent extends LocalVariablesComponent {
  (cunit: CompilationUnitApi) => {
    subst((cunit.module, env))
  }
}

@component(tree, env)
trait ClassDefLocalVariablesComponent extends LocalVariablesComponent {
  (clazz: ClassDefApi) => {
    subst((clazz.body, env))
  }
}

@component(tree, env)
trait MethodDefLocalVariablesComponent extends LocalVariablesComponent {
  (mthd: MethodDefApi) => {
    mthd.symbol match {
      case Some(sym)                               =>
        env.newMethod(sym)
        mthd.params.foreach(p => subst((p, env)))
        subst((mthd.body, env))
        val max = 0.max(env.getMax)
        mthd.locals = max
      case _                                       =>
        ()
    }
  }
}

@component(tree, env)
trait ValDefLocalVariablesComponent extends LocalVariablesComponent {
  (valdef: ValDefApi) => {
    subst((valdef.rhs, env))
    valdef.symbol.foreach { sym =>
      if(!sym.mods.isField) {
        env.add(sym)
        for {
          symbol <- valdef.symbol
          index  <- env.getIndex(symbol)
        } {
          valdef.variableIndex = index
        }
      }
    }
    ()
  }
}

@component(tree, env)
trait SynchronizedLocalVariablesComponent extends LocalVariablesComponent {
  (synch: SynchronizedApi) => {
    env.advanceOnce
    val start = env.currentIndex
    subst((synch.expr, env))
    subst((synch.block, env))
    val end = env.currentIndex
    env.advanceOnce
    synch.identifierIndices = (start, end)
  }
}

@component(tree, env)
trait TryLocalVariablesComponent extends LocalVariablesComponent {
  (tri: TryApi) => {
    val tryEnv = env.duplicate
    subst((tri.tryClause, tryEnv))
    val tryMax = tryEnv.getMax
    val currentMax = tri.catches.foldLeft(tryMax) (
      (z, y) => {
        val cenv = env.duplicate
        subst((y, cenv))
        z.max(cenv.getMax)
      })
    tri.finallyClause match {
      case Some(block)               =>
        env.newCurrentIndex(currentMax)
        tri.finallyParamIndex = env.currentIndex
        env.advanceOnce
        subst((block, env))
      case _                         =>
        env.newMax(currentMax)
    }
  }
}


@component(tree, env)
trait IdentLocalVariablesComponent extends LocalVariablesComponent {
  (ident: IdentApi) => {
    for {
      symbol <- ident.symbol
      index  <- env.getIndex(symbol)
    } {
      ident.identifierIndex = index
    }
  }
}


@component(tree, env)
trait BlockLocalVariablesComponent extends LocalVariablesComponent {
  (block: BlockApi) => {
    block.stmts.foreach(s => subst((s, env)))
  }
}


@component(tree, env)
trait CastLocalVariablesComponent extends LocalVariablesComponent {
  (cast: CastApi) => {
    subst((cast.expr, env))
  }
}

@component(tree, env)
trait UnaryLocalVariablesComponent extends LocalVariablesComponent {
  (unary: UnaryApi) => {
    subst((unary.expr, env))
  }
}

@component(tree, env)
trait BinaryLocalVariablesComponent extends LocalVariablesComponent {
  (bin: BinaryApi) => {
    subst((bin.lhs, env))
    subst((bin.rhs, env))
  }
}

@component(tree, env)
trait ApplyLocalVariablesComponent extends LocalVariablesComponent {
  (app: ApplyApi) => {
    subst((app.fun, env))
    app.args.foreach(arg => subst((arg, env)))
  }
}

@component(tree, env)
trait AssignLocalVariablesComponent extends LocalVariablesComponent {
  (assign: AssignApi) => {
    subst((assign.lhs, env))
    subst((assign.rhs, env))
  }
}


@component(tree, env)
trait TernaryLocalVariablesComponent extends LocalVariablesComponent {
  (tern: TernaryApi) => {
    subst((tern.cond, env))
    subst((tern.thenp, env))
    subst((tern.elsep, env))
  }
}

@component(tree, env)
trait NewLocalVariablesComponent extends LocalVariablesComponent {
  (nw: NewApi) => {
    subst((nw.app, env))
  }
}


@component(tree, env)
trait SelectLocalVariablesComponent extends LocalVariablesComponent {
  (select: SelectApi) => {
    subst((select.qual, env))
  }
}

@component(tree, env)
trait ThrowLocalVariablesComponent extends LocalVariablesComponent {
  (thrw: ThrowApi) => {
    subst((thrw.expr, env))
  }
}


@component(tree, env)
trait CatchLocalVariablesComponent extends LocalVariablesComponent {
  (ctch: CatchApi) => {
    subst((ctch.eparam, env))
    subst((ctch.catchClause, env))
  }
}


@component(tree, env)
trait ArrayAccessLocalVariablesComponent extends LocalVariablesComponent {
  (access: ArrayAccessApi) => {
    subst((access.array, env))
    subst((access.index, env))
  }
}


@component(tree, env)
trait ArrayInitializerLocalVariablesComponent extends LocalVariablesComponent {
  (array: ArrayInitializerApi) => {
    array.elements.foreach(elem => subst((elem, env)))
  }
}



@component(tree, env)
trait ReturnLocalVariablesComponent extends LocalVariablesComponent {
  (ret: ReturnApi) => {
    ret.expr.foreach(e => subst((e, env)))
  }
}

@component(tree, env)
trait LabelLocalVariablesComponent extends LocalVariablesComponent {
  (lbl: LabelApi) => {
    subst((lbl.stmt, env))
  }
}


@component(tree, env)
trait SwitchLocalVariablesComponent extends LocalVariablesComponent {
  (switch: SwitchApi) => {
    subst((switch.expr, env))
    switch.cases.foreach(cse => subst((cse, env)))
  }
}



@component(tree, env)
trait CaseLocalVariablesComponent extends LocalVariablesComponent {
  (cse: CaseApi) => {
    subst((cse.body, env))
  }
}


@component(tree, env)
trait WhileLocalVariablesComponent extends LocalVariablesComponent {
  (wile: WhileApi) => {
    val wenv = env.duplicate
    subst((wile.cond, wenv))
    subst((wile.body, wenv))
    env.newMax(wenv.getMax)
  }
}


@component(tree, env)
trait ForLocalVariablesComponent extends LocalVariablesComponent {
  (forloop: ForApi) => {
    val fenv = env.duplicate
    forloop.inits.foreach(init => subst((init, fenv)))
    subst((forloop.cond, fenv))
    forloop.steps.foreach(step => subst((step, fenv)))
    subst((forloop.body, fenv))
    env.newMax(fenv.getMax)
  }
}


@component(tree, env)
trait IfLocalVariablesComponent extends LocalVariablesComponent {
  (ifelse: IfApi) => {
    subst((ifelse.cond, env))
    val tenv = env.duplicate
    val eenv = env.duplicate
    subst((ifelse.thenp, tenv))
    subst((ifelse.elsep, eenv))
    val currentMax  = tenv.getMax.max(eenv.getMax)
    env.newMax(currentMax)
  }
}
