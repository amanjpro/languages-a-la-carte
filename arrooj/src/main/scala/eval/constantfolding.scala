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

package ch.usi.inf.l3.sana.arrooj.eval

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


import ooj.eval.ConstantFoldingComponent
import arrayj.ast._
import tiny.ast.{UseTree, Expr}
import arrooj.ast.Implicits._
import arrooj.types.TypeUtils
import arrooj.symbols.SymbolUtils

/*
ArrayTypeUse: DONE
ArrayCreation: DONE
ArrayInitializer: DONE
ArrayAccess: DONE
*/
@component(tree, env)
trait ArrayTypeUseConstantFoldingComponent
  extends ConstantFoldingComponent {
  (tuse: ArrayTypeUseApi) => {
    val (tpt, env1) = constantFold((tuse.tpt, env))
    val res =
      TreeCopiers.copyArrayTypeUse(tuse)(tpt = tpt.asInstanceOf[UseTree])
    tpt.tpe.foreach { tpe =>
      res.tpe = TypeUtils.mkArrayType(tpe)
    }
    tpt.symbol.foreach { sym =>
      res.symbol = SymbolUtils.mkArraySymbol(sym)
    }
    (res, env1)
  }
}

@component(tree, env)
trait ArrayCreationConstantFoldingComponent
  extends ConstantFoldingComponent {
  (creation: ArrayCreationApi) => {
    val (array, env1) = constantFold((creation.array, env))
    creation.size map { size =>
      val (size2, env2) = constantFold((size, env1))
      val res =
        TreeCopiers.copyArrayCreation(creation)(
          array = array.asInstanceOf[Expr],
          size = Some(size2.asInstanceOf[Expr]))
      (res, env2)
    } getOrElse {
        val res = TreeCopiers.copyArrayCreation(creation)(
          array = array.asInstanceOf[Expr])
        (res, env1)
    }
  }
}


@component(tree, env)
trait ArrayInitializerConstantFoldingComponent
  extends ConstantFoldingComponent {
  (init: ArrayInitializerApi) => {
    val zero: List[Expr] = Nil
    val (stnemele, env1) = init.elements.foldLeft((zero, env))((z, y) => {
      val zelements = z._1
      val zenv      = z._2
      val (y1, y2)  = constantFold((y, zenv))
      ((y1.asInstanceOf[Expr]::zelements, y2))
    })
    val res =
      TreeCopiers.copyArrayInitializer(init)(elements = stnemele.reverse)
    (res, env1)
  }
}

@component(tree, env)
trait ArrayAccessConstantFoldingComponent
  extends ConstantFoldingComponent {
  (access: ArrayAccessApi) => {
    val (array, env1) = constantFold((access.array, env))
    val (index, env2) = constantFold((access.index, env1))
    val res =
      TreeCopiers.copyArrayAccess(access)(array = array.asInstanceOf[Expr],
        index = index.asInstanceOf[Expr])
    (res, env1)
  }
}
