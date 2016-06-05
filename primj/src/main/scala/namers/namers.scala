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

package ch.usi.inf.l3.sana.primj.namers

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.TransformationComponent
import tiny.dsl._
import tiny.ast.{TreeCopiers => _, _}
import primj.ast.Implicits._
import tiny.symbols._
import calcj.ast.{TreeCopiers => _, _}
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.ast._
import primj.ast.TreeFactories._
import primj.ast.TreeUtils
import primj.symbols._
import primj.types._
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._

/*
Program: DONE
Assign: DONE
If: DONE
While: DONE
Block: DONE
For: DONE
Ternary: DONE
Apply: DONE
Return: DONE
MethodDef: DONE
ValDef: DONE
Ident: DONE
NoTree: DONE
TypeUse: DONE
Cast: DONE
Binary: DONE
Literal: DONE
Unary: DONE
*/


trait NamerComponent extends TransformationComponent[Tree, Tree] {
  def name: Tree => Tree
}

@component
trait ProgramNamerComponent extends NamerComponent {
  (program: ProgramApi)          => {
    val newMembers =
      program.members.map(x => name(x).asInstanceOf[DefTree])
    TreeCopiers.copyProgram(program)(members = newMembers)
  }

}

@component
trait MethodDefNamerComponent extends NamerComponent {
  (mthd: MethodDefApi)          => {
    // val rhs    = name(mthd.body).asInstanceOf[Expr]
    val tpt    = name(mthd.ret).asInstanceOf[UseTree]
    val params = mthd.params.map(name(_).asInstanceOf[ValDefApi])
    val mtpe    = {
      val ptpes = params.flatMap(_.tpe)
      if(ptpes.size == params.size) {
        for {
          rtpe <- tpt.tpe
        } yield MethodType(rtpe, ptpes)
      } else None
    }
    mthd.symbol.foreach(_.tpe = mtpe)
    mtpe.foreach(mthd.tpe = _)


    TreeCopiers.copyMethodDef(mthd)(//body = rhs,
      ret = tpt, params = params)
  }

}

@component
trait ValDefNamerComponent extends NamerComponent {
  (valdef: ValDefApi)          => {
    // Only local variables need to be named
    val tpt     = name(valdef.tpt).asInstanceOf[UseTree]
    // val rhs     = name(valdef.rhs).asInstanceOf[Expr]
    valdef.symbol.foreach(sym => {
      sym.tpe.foreach(valdef.tpe = _)
      sym match {
        case vs: VariableSymbol =>
          vs.typeSymbol = tpt.symbol
        case _                  =>
          ()
      }
    })
    TreeCopiers.copyValDef(valdef)(tpt = tpt)//, rhs = rhs)
  }

}


@component
trait TypeUseNamerComponent extends NamerComponent {
  (tuse: TypeUseApi)          => {
    tuse.hasBeenNamed = true
    val symbol = tuse.owner.flatMap(_.getSymbol(tuse.name,
      _.isInstanceOf[TypeSymbol]))
    symbol match {
      case Some(sym: TypeSymbol)      =>
        tuse.symbol = sym
        sym.tpe.foreach(tuse.tpe = _)
        tuse
      case Some(_)                    =>
        error(TYPE_NAME_EXPECTED,
          tuse.toString, "a type", tuse.pos)
        tuse
      case _                          =>
        error(TYPE_NOT_FOUND,
          tuse.toString, "a type", tuse.pos)
        tuse
    }
  }

}

@component
trait IdentNamerComponent extends NamerComponent {
  (id: IdentApi)          => {
    id.hasBeenNamed = true
    val symbol = id.owner.flatMap(_.getSymbol(id.name,
      _.isInstanceOf[TermSymbol]))
    symbol match {
      case Some(sym)      =>
        id.symbol = sym
        sym.tpe.foreach(id.tpe = _)
        id
      case _              =>
        error(NAME_NOT_FOUND,
          id.toString, "a term", id.pos)
        id
    }
  }

}
//
// @component
// trait ForNamerComponent extends NamerComponent {
//   (forloop: ForApi)          => {
//     val inits = forloop.inits.map { init =>
//       name(init)
//     }
//     val cond = name(forloop.cond).asInstanceOf[Expr]
//     val steps = forloop.steps.map { step =>
//       name(step).asInstanceOf[Expr]
//     }
//     val body = name(forloop.body).asInstanceOf[Expr]
//     TreeCopiers.copyFor(forloop)(inits = inits, cond = cond,
//       steps = steps, body = body)
//   }
//
// }
//
// @component
// trait BlockNamerComponent extends NamerComponent {
//   (block: BlockApi)          => {
//     val stmts = block.stmts.map { stmt => name(stmt) }
//     TreeCopiers.copyBlock(block)(stmts = stmts)
//   }
//
// }
// // Boring cases, just pass the owner around and name it to
// // all the trees that can have an owner
//
//
// @component
// trait BinaryNamerComponent extends NamerComponent {
//   (bin: BinaryApi)          => {
//     val lhs = name(bin.lhs).asInstanceOf[Expr]
//     val rhs = name(bin.rhs).asInstanceOf[Expr]
//     TreeCopiers.copyBinary(bin)(lhs = lhs, rhs = rhs)
//   }
//
// }
//
// @component
// trait UnaryNamerComponent extends NamerComponent {
//   (unary: UnaryApi)          => {
//     val expr = name(unary.expr).asInstanceOf[Expr]
//     TreeCopiers.copyUnary(unary)(expr = expr)
//   }
//
// }
//
// @component
// trait CastNamerComponent extends NamerComponent {
//   (cast: CastApi)          => {
//     val expr = name(cast.expr).asInstanceOf[Expr]
//     TreeCopiers.copyCast(cast)(expr = expr)
//   }
//
// }
//
// @component
// trait ReturnNamerComponent extends NamerComponent {
//   (ret: ReturnApi)          => {
//     val expr = ret.expr.map(name(_).asInstanceOf[Expr])
//     TreeCopiers.copyReturn(ret)(expr = expr)
//   }
//
// }
//
// @component
// trait AssignNamerComponent extends NamerComponent {
//   (assgn: AssignApi)          => {
//     val lhs = name(assgn.lhs).asInstanceOf[Expr]
//     val rhs = name(assgn.rhs).asInstanceOf[Expr]
//     TreeCopiers.copyAssign(assgn)(lhs = lhs, rhs = rhs)
//   }
//
// }
//
//
// @component
// trait TernaryNamerComponent extends NamerComponent {
//   (tern: TernaryApi)          => {
//     val cond = name(tern.cond).asInstanceOf[Expr]
//     val thenp = name(tern.thenp).asInstanceOf[Expr]
//     val elsep = name(tern.elsep).asInstanceOf[Expr]
//     TreeCopiers.copyTernary(tern)(cond = cond,
//       thenp = thenp, elsep = elsep)
//   }
//
// }
//
// @component
// trait IfNamerComponent extends NamerComponent {
//   (ifelse: IfApi)          => {
//     val cond = name(ifelse.cond).asInstanceOf[Expr]
//     val thenp = name(ifelse.thenp).asInstanceOf[Expr]
//     val elsep = name(ifelse.elsep).asInstanceOf[Expr]
//     TreeCopiers.copyIf(ifelse)(cond = cond, thenp = thenp,
//       elsep = elsep)
//   }
//
// }
//
// @component
// trait WhileNamerComponent extends NamerComponent {
//   (wile: WhileApi)          => {
//     val cond = name(wile.cond).asInstanceOf[Expr]
//     val body = name(wile.body).asInstanceOf[Expr]
//     TreeCopiers.copyWhile(wile)(cond = cond, body = body)
//   }
//
// }
//
// @component
// trait ApplyNamerComponent extends NamerComponent {
//   (apply: ApplyApi)          => {
//     val fun = name(apply.fun).asInstanceOf[Expr]
//     val args = apply.args.map { arg =>
//       name(arg).asInstanceOf[Expr]
//     }
//     TreeCopiers.copyApply(apply)(fun = fun, args = args)
//   }
//
// }
//
// @component
// trait LiteralNamerComponent extends NamerComponent {
//   (lit: LiteralApi)          => lit
// }
//
//
