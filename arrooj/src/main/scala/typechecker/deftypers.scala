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

package ch.usi.inf.l3.sana.arrooj.typechecker


import ch.usi.inf.l3.sana
import sana.arrooj
import sana.arrayj
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.TransformationComponent
import tiny.dsl._


import arrooj.types._
import arrooj.symbols.SymbolUtils
import arrooj.types.TypeUtils
import arrayj.ast.{TreeUtils => _, TreeCopiers => _, _}
import arrooj.ast._
import arrooj.ast.Implicits._
import ooj.typechecker.DefTyperComponent
import tiny.types.Type
import tiny.ast.{UseTree, Tree}


/*
ArrayTypeUse: DONE
*/


@component
trait ArrayTypeUseDefTyperComponent extends DefTyperComponent {
  (tuse: ArrayTypeUseApi) => {
    val tpt = typed(tuse.tpt).asInstanceOf[UseTree]
    tpt.symbol.foreach { sym =>
      tuse.symbol = SymbolUtils.mkArraySymbol(sym)
    }
    tpt.tpe.foreach { tpe =>
      tuse.tpe = TypeUtils.mkArrayType(tpe)
    }
    TreeCopiers.copyArrayTypeUse(tuse)(tpt = tpt)
  }
}

@component
trait SelectDefTyperComponent
  extends ooj.typechecker.SelectDefTyperComponent {
  /** @see [[ooj.typechecker.SelectDefTyperComponent]] */
  override protected def isTypeUse(tree: Tree): Boolean = tree match {
    case t: UseTree => TreeUtils.isTypeUse(t)
    case _          => false
  }
}
