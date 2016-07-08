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

package ch.usi.inf.l3.sana.ooj.eval

import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.TransformationComponent
import tiny.dsl._


import tiny.symbols.Symbol
import ooj.ast._
import ooj.ast.Implicits._
import ooj.ast.TreeExtractors._
import ooj.modifiers.Ops._
import ooj.symbols.SymbolUtils
import brokenj.ast.{TreeCopiers => _, TreeUtils => _, _}
import primj.ast.{TreeCopiers => _, MethodDefApi => PMethodDefApi,
                  ProgramApi => _, TreeUtils => _, _}
import calcj.ast.{TreeCopiers => _, _}
import tiny.ast.{TreeCopiers => _, _}



trait ConstantCollectingComponent
  extends TransformationComponent[(Tree, Env), Env] {
  def collect: ((Tree, Env)) => Env
}


@component(tree, env)
trait ProgramConstantCollectingComponent extends ConstantCollectingComponent {
  (prg: ProgramApi)  => {
    prg.members.foldLeft(env){
      (z, member) =>
        collect((member, z))
    }
  }
}

@component(tree, env)
trait CompilationUnitConstantCollectingComponent extends
    ConstantCollectingComponent {
  (cunit: CompilationUnitApi)  => {
    collect((cunit.module, env))
  }
}

@component(tree, env)
trait PackageDefConstantCollectingComponent
  extends ConstantCollectingComponent {
  (pkg: PackageDefApi)  => {
    pkg.members.foldLeft(env) {
      (z, member) =>
        collect((member, z))

    }
  }
}

@component(tree, env)
trait ClassDefConstantCollectingComponent
  extends ConstantCollectingComponent {
  (clazz: ClassDefApi)  => {
    collect((clazz.body, env))
  }
}

@component(tree, env)
trait TemplateConstantCollectingComponent
  extends ConstantCollectingComponent {
  (template: TemplateApi)  => {
    template.members.foldLeft(env){
    (z, member) =>
      collect((member, z))
    }
  }
}


@component(tree, env)
trait ValDefConstantCollectingComponent
  extends ConstantCollectingComponent {
  (valdef: ValDefApi) => {
    if(valdef.mods.isFinal && valdef.mods.isField &&
        valdef.rhs != NoTree ) {
      val newEnv2 = valdef.symbol.map { sym =>
        env.bind(sym, ExprValue(valdef.rhs))
      }.getOrElse(env)
      newEnv2
    } else env
  }

}



@component(tree, env)
trait MethodDefConstantCollectingComponent
  extends ConstantCollectingComponent {
  (mthd: PMethodDefApi) => env
}

@component(tree, env)
trait BlockConstantCollectingComponent
  extends ConstantCollectingComponent {
  (block: BlockApi) => env
}

// @component(tree, env)
// trait SelectConstantCollectingComponent
//   extends ConstantCollectingComponent {
//   (select: SelectApi) => {
//     if(isTypeSymbol(select.qual.symbol) &&
//         isStatic(select.tree.symbol)) {
//       select.symbol.map { sym =>
//         env.getValue(sym) match {
//           case TypeValue(tenv)          =>
//             val (v, _) = collect((select.tree, tenv))
//             if(isConstantExpression(v)) {
//               (v, env)
//             } else {
//               (select, env)
//             }
//           case _                        =>
//             (select, env)
//         }
//       }.getOrElse((select, env))
//     } else {
//       (select, env)
//     }
//   }
//
//
//   def isTypeSymbol(sym: Option[Symbol]): Boolean =
//     SymbolUtils.isTypeSymbol(sym)
//
//   def isStatic(sym: Option[Symbol]): Boolean =
//     sym.map(_.mods.isStatic).getOrElse(false)
//
//   protected def isConstantExpression(tree: Tree): Boolean =
//     TreeUtils.isConstantExpression(tree)
// }
//
//
// @component(tree, env)
// trait IdentConstantCollectingComponent
//   extends ConstantCollectingComponent {
//   (ident: IdentApi)    => {
//     ident.symbol.map { sym =>
//       env.getValue(sym) match {
//         case ExprValue(v) =>
//           (v, env)
//         case _            =>
//           (ident, env)
//       }
//     }.getOrElse((ident, env))
//   }
//
//   protected def isConstantExpression(tree: Tree): Boolean =
//     TreeUtils.isConstantExpression(tree)
// }
