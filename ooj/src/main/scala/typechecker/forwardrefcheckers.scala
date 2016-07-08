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

package ch.usi.inf.l3.sana.ooj.typechecker

import ch.usi.inf.l3.sana
import sana.ooj
import sana.primj
import sana.tiny


import ooj.ast._
import primj.ast.{BlockApi, ValDefApi}
import tiny.ast.{Tree, IdentApi}
import tiny.symbols.Symbol
import ooj.modifiers.Ops._
import ooj.ast.Implicits._
import ooj.symbols.SymbolUtils
import tiny.errors.ErrorReporting.{error,warning}
import ooj.errors.ErrorCodes._

import tiny.dsl._
import tiny.core._



trait ForwardRefCheckerComponent
  extends CheckerComponent[(Tree, List[Symbol])] {
  def check: ((Tree, List[Symbol])) => Unit
}



@component(tree, symbols)
trait ProgramForwardRefCheckerComponent extends ForwardRefCheckerComponent {
  (prg: ProgramApi) => {
    prg.members.foreach(x => check((x, symbols)))
  }
}

@component(tree, symbols)
trait CompilationUnitForwardRefCheckerComponent
  extends ForwardRefCheckerComponent {
  (unit: CompilationUnitApi) => {
    check((unit.module, symbols))
  }
}

@component(tree, symbols)
trait PackageDefForwardRefCheckerComponent
  extends ForwardRefCheckerComponent {
  (pkg: PackageDefApi) => {
    pkg.members.foreach(x => check((x, symbols)))
  }
}

@component(tree, symbols)
trait ClassDefForwardRefCheckerComponent extends ForwardRefCheckerComponent {
  (clazz: ClassDefApi) => {
    check((clazz.body, symbols))
  }
}

@component(tree, symbols)
trait TemplateForwardRefCheckerComponent extends ForwardRefCheckerComponent {
  (template: TemplateApi) => {
    template.members.foldLeft(Nil: List[Symbol]) { (symbols, member) =>
      member match {
        case v: ValDefApi                    if v.mods.isField =>
          v.rhs.foreach {
            case id: IdentApi                    =>
              id.symbol.foreach { sym =>
                if(!id.isQualified && !symbols.contains(sym) &&
                      v.owner == sym.owner &&
                      sym.mods.isField)
                  error(ILLEGAL_FORWARD_REFERENCE,
                    id.name.asString, "", id.pos)
              }
            case _                               =>
              ()
          }
          v.symbol.map(_::symbols).getOrElse(symbols)
        case block: BlockApi if block.isStaticInit             =>
          check((block, symbols))
          symbols
        case _                                                 =>
          symbols
      }
    }
  }
}


@component(tree, symbols)
trait BlockForwardRefCheckerComponent extends ForwardRefCheckerComponent {
  (block: BlockApi) => {
    val clazz = enclosingClass(block.owner)
    if(block.isStaticInit) {
      block.bottomUp(()) ( (z, t) => t match {
          case id: IdentApi                    =>
            id.symbol.foreach { sym =>
              if(sym.mods.isField && sym.mods.isStatic &&
                  clazz == sym.owner &&
                  !symbols.contains(sym))
                error(FIELD_FORWARD_REFERENCE_IN_STATIC_INIT,
                  "", "", t.pos)
            }
          case _                               =>
            ()
        }
      )
    }
  }

  protected def enclosingClass(sym: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(sym)
}
