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

package ch.usi.inf.l3.sana.arrayj.namers

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj
import sana.arrayj

import tiny.core.TransformationComponent
import tiny.dsl._
import tiny.ast.{TreeCopiers => _, _}
import primj.ast.Implicits._
import tiny.errors.ErrorReporting.{error,warning}
import tiny.symbols._
import calcj.ast.{TreeCopiers => _, _}
import primj.ast.{TreeCopiers => _, _}
import primj.symbols._
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._
import primj.namers.SymbolAssignerComponent
import arrayj.ast._

/*
ArrayCreation: DONE
ArrayAccess: DONE
ArrayTypeUse: DONE
ArrayInitializer: DONE
*/


@component
trait ArrayCreationSymbolAssignerComponent extends SymbolAssignerComponent {
  (creation: ArrayCreationApi)     => {
    val owner = creation.owner
    owner.foreach { owner =>
      creation.array.owner = owner
      creation.size.foreach(_.owner = owner)
    }

    val array = assign(creation.array).asInstanceOf[Expr]
    val size  = creation.size.map(assign(_).asInstanceOf[Expr])
    TreeCopiers.copyArrayCreation(creation)(array = array, size = size)
  }
}

@component
trait ArrayAccessSymbolAssignerComponent extends SymbolAssignerComponent {
  (access: ArrayAccessApi)     => {
    val owner = access.owner
    owner.foreach { owner =>
      access.array.owner = owner
      access.index.owner = owner
    }

    val array = assign(access.array).asInstanceOf[Expr]
    val index = assign(access.index).asInstanceOf[Expr]
    TreeCopiers.copyArrayAccess(access)(array = array, index = index)
  }
}

@component
trait ArrayInitializerSymbolAssignerComponent extends SymbolAssignerComponent {
  (init: ArrayInitializerApi)     => {
    val owner = init.owner
    val elements = init.elements.map { elem =>
      owner.foreach(elem.owner = _)
      assign(elem).asInstanceOf[Expr]
    }
    TreeCopiers.copyArrayInitializer(init)(elements = elements)
  }
}

@component
trait ArrayTypeUseSymbolAssignerComponent extends SymbolAssignerComponent {
  (tuse: ArrayTypeUseApi)     => {
    val owner = tuse.owner
    owner.foreach { owner =>
      tuse.tpt.owner = owner
    }

    val tpt = assign(tuse.tpt).asInstanceOf[UseTree]
    TreeCopiers.copyArrayTypeUse(tuse)(tpt = tpt)
  }
}
