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
import sana.primj
import sana.tiny


import ooj.ast._
import primj.ast.{BlockApi, ValDefApi, AssignApi}
import tiny.ast.{Tree, IdentApi, NoTree}
import tiny.symbols.Symbol
import ooj.modifiers.Ops._
import ooj.ast.Implicits._
import ooj.ast.TreeExtractors._
import ooj.symbols.SymbolUtils
import tiny.errors.ErrorReporting.{error,warning}
import ooj.errors.ErrorCodes._
import scala.collection.mutable

import tiny.dsl._
import tiny.core._


class ConstructorCheckerEnv (private val constructorGraph:
        mutable.Map[Symbol, Symbol] = mutable.Map.empty) {

  private var uninitializedFinalFields: List[Symbol]              = Nil
  private var defaultInitFinalFields: List[Symbol]                = Nil



  def getAllUninitializedFields: List[Symbol] = {
    uninitializedFinalFields.diff(defaultInitFinalFields)
  }



  def hasDefaultInit(sym: Symbol): Boolean =
    defaultInitFinalFields.contains(sym)

  def addDefaultInitFinalField(sym: Symbol): Unit =
    defaultInitFinalFields = sym::defaultInitFinalFields
  def removeDefaultInitFinalField(sym: Symbol): Unit =
    defaultInitFinalFields = defaultInitFinalFields.filter(_ != sym)


  def isInitialized(sym: Symbol): Boolean = {
    !uninitializedFinalFields.contains(sym)
  }

  def initializeAllFields(): Unit = uninitializedFinalFields = Nil

  def initializeField(sym: Symbol): Unit =
    removeUninitializedFinalField(sym)

  def addUninitializedFinalField(sym: Symbol): Unit =
    uninitializedFinalFields = sym::uninitializedFinalFields
  def removeUninitializedFinalField(sym: Symbol): Unit =
    uninitializedFinalFields = uninitializedFinalFields.filter(_ != sym)

  def connectConstructor(source: Symbol, dest: Symbol): Unit = {
    constructorGraph += (source -> dest)
  }

  def isCyclic(source: Symbol): Boolean = {
    val r = constructorGraph.get(source).map { dest =>
      isThereABackLink(source, dest, Nil)
    }
    r.getOrElse(false)
  }

  private def isThereABackLink(source: Symbol, dest: Symbol,
                   seen: List[Symbol] = Nil): Boolean = {
    constructorGraph.get(dest) match {
      case None                                        =>
        false
      case Some(candidate)  if candidate == source     =>
        true
      case Some(candidate)                             =>
        if(!seen.contains(candidate))
          isThereABackLink(source, candidate, dest::seen)
        else {
          false
        }
    }
  }


  def duplicate: ConstructorCheckerEnv = {
    val temp = new ConstructorCheckerEnv(this.constructorGraph)
    temp.uninitializedFinalFields = this.uninitializedFinalFields
    temp.defaultInitFinalFields   = this.defaultInitFinalFields
    temp
  }
}

trait ConstructorsCheckerComponent
  extends CheckerComponent[(Tree, ConstructorCheckerEnv)] {
  def check: ((Tree, ConstructorCheckerEnv)) => Unit
}



@component(tree, env)
trait ProgramConstructorsCheckerComponent extends
    ConstructorsCheckerComponent {
  (prg: ProgramApi) => {
    prg.members.foreach(x => check((x, env)))
  }
}

@component(tree, env)
trait CompilationUnitConstructorsCheckerComponent
  extends ConstructorsCheckerComponent {
  (unit: CompilationUnitApi) => {
    check((unit.module, env))
  }
}

@component(tree, env)
trait PackageDefConstructorsCheckerComponent
  extends ConstructorsCheckerComponent {
  (pkg: PackageDefApi) => {
    pkg.members.foreach(x => check((x, env)))
  }
}

@component(tree, env)
trait ClassDefConstructorsCheckerComponent extends
  ConstructorsCheckerComponent {
  (clazz: ClassDefApi) => {
    check((clazz.body, new ConstructorCheckerEnv))
  }
}

@component(tree, env)
trait TemplateConstructorsCheckerComponent extends
  ConstructorsCheckerComponent {
  (template: TemplateApi) => {
    collectFields(template.members, env)
    collectInits(template.members, env)
    collectConstructors(template.members, env)
    checkCyclicConstructors(template.members, env)
  }

  protected def collectFields(members: List[Tree],
          env: ConstructorCheckerEnv): Unit = {
    members.foreach { member =>
      member match {
        case v: ValDefApi  if v.mods.isField && v.mods.isFinal &&
                              (v.rhs == NoTree ||
                               (v.mods.isStatic && v.hasDefaultInit))  =>
          if(v.hasDefaultInit && v.mods.isStatic)
            v.symbol.foreach(env.addDefaultInitFinalField(_))
          else v.symbol.foreach(env.addUninitializedFinalField(_))
        case _                                                         =>
          ()
      }
    }
  }

  protected def collectInits(members: List[Tree],
          env: ConstructorCheckerEnv): Unit = {
    members.foreach { member =>
      member match {
        case init: BlockApi  if init.isStaticInit    =>
          check((init, env))
        case _                                       =>
          ()
      }
    }
  }

  protected def collectConstructors(members: List[Tree],
          env: ConstructorCheckerEnv): Unit = {
    members.foreach { member =>
      member match {
        case cnstr: MethodDefApi  if cnstr.mods.isConstructor =>
          check((cnstr, env.duplicate))
        case _                                               =>
          ()
      }
    }
  }

  protected def checkCyclicConstructors(members: List[Tree],
          env: ConstructorCheckerEnv): Unit = {
    members.foreach { member =>
      member match {
        case cnstr: MethodDefApi  if cnstr.mods.isConstructor =>
          cnstr.symbol.foreach { sym =>
            if(env.isCyclic(sym)) {
              error(CYCLIC_CONSTRUCTOR_CALL,
                "", "", cnstr.pos)
            }
          }
        case _                                                =>
          ()
      }
    }
  }
}


@component(tree, env)
trait BlockConstructorsCheckerComponent extends
  ConstructorsCheckerComponent {
  (block: BlockApi) => {
    block.stmts.foreach(stmt => {
      stmt match {
        case _: AssignApi       =>
          check((stmt, env))
        case _                  =>
          ()
      }
    })
  }
}


@component(tree, env)
trait MethodDefConstructorsCheckerComponent extends
  ConstructorsCheckerComponent {
  (mthd: MethodDefApi) => {
    if(mthd.mods.isConstructor) {
      mthd.body match {
        case Block(Apply(c@Select(_: ThisApi, _), _)::stmts)  =>
          checkBody(mthd.symbol, c.symbol, true, env, stmts)
        case Block(Apply(c@Select(_: SuperApi, _), _)::stmts) =>
          checkBody(mthd.symbol, c.symbol, false, env, stmts)
        case _                                                =>
          ()
      }

      env.getAllUninitializedFields match {
        case Nil                          =>
          ()
        case fields                       =>
          error(FINAL_FIELDS_MIGHT_NOT_BE_INITIALIZED,
            fields.map(_.name).mkString(", "), "", mthd.pos)
      }
    }
  }


  protected def checkBody(sym: Option[Symbol],
                          explicitConstructor: Option[Symbol],
                          invokesThis: Boolean,
                          env: ConstructorCheckerEnv,
                          stmts: List[Tree]): Unit = {
    for {
      sym  <- sym
      csym <- explicitConstructor if invokesThis
    } {
      env.initializeAllFields
      env.connectConstructor(sym, csym)
    }
    stmts.map {
      case stmt: AssignApi       =>
        check((stmt, env))
      case _                     =>
        ()
    }
  }
}


@component(tree, env)
trait AssignConstructorsCheckerComponent extends
  ConstructorsCheckerComponent {
  (assign: AssignApi) => {
    assign.lhs.symbol.foreach { sym =>
      if(sym.mods.isFinal && sym.mods.isField) {
        enclosingNonLocal(assign.owner).foreach { encl =>
          if(env.isInitialized(sym)) {
            if(env.hasDefaultInit(sym) &&
                encl.mods.isStaticInit)
              env.removeDefaultInitFinalField(sym)
            else
              error(FINAL_FIELD_IS_ALREADY_INITIALIZED,
                "", "", assign.pos)
          } else {
              env.initializeField(sym)
          }
        }
      }
    }
  }

  protected def enclosingNonLocal(owner: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingNonLocal(owner)
}
