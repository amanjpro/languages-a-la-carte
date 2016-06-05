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
import tiny.names.Name
import tiny.source.Position
import primj.ast.Implicits._
import tiny.errors.ErrorReporting.{error,warning}
import tiny.symbols._
import calcj.ast.{TreeCopiers => _, _}
import calcj.ast.operators.{Inc, Dec}
import primj.ast._
import primj.ast.TreeUtils
import primj.ast.TreeFactories._
import primj.symbols._
import primj.modifiers.Ops._
import primj.modifiers._
import primj.errors.ErrorCodes._


// package symbolassigners {
//   import scala.language.implicitConversions
//   object Implicits {
//     // We need this to glue compilation phases together easier
//     implicit def tree2TreeOptionSymbolTuple(tree: Tree):
//       (Tree, Option[Symbol]) = (tree, None)
//   }
// }


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

trait SymbolAssignerComponent extends
  TransformationComponent[Tree, Tree] {
  def assign: Tree => Tree
}

@component
trait ProgramSymbolAssignerComponent extends SymbolAssignerComponent {
  (program: ProgramApi)          => {
    val symbol = ProgramSymbol
    val newMembers =
      program.members.map { x =>
        x.owner = symbol
        assign(x).asInstanceOf[DefTree]
    }
    program.symbol = symbol
    TreeCopiers.copyProgram(program)(members = newMembers)
  }
}


@component
trait MethodDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (mthd: MethodDefApi)          => {
    val owner   = mthd.owner
    val symbol  = MethodSymbol(noflags, mthd.name, None,
      Nil, None, owner)
    checkDoubleDef(owner, mthd.name, mthd.pos)
    owner.foreach(sym => sym.declare(symbol))
    owner.foreach(mthd.ret.owner  = _)
    mthd.body.owner = symbol
    val tpt     = assign(mthd.ret).asInstanceOf[UseTree]
    val params  = mthd.params.map { param =>
      val p = if(!param.mods.isParam) {
        val mods = param.mods | PARAM
        TreeCopiers.copyValDef(param)(mods = mods)
      } else param
      p.owner = symbol
      assign(p).asInstanceOf[ValDefApi]
    }
    val body    = assign(mthd.body).asInstanceOf[Expr]
    symbol.params = params.map(_.symbol).flatten
    symbol.ret    = tpt.symbol
    mthd.symbol   = symbol
    symbol.owner.foreach(mthd.owner = _)
    TreeCopiers.copyMethodDef(mthd)(ret = tpt,
      params = params, body = body)
  }

  protected def checkDoubleDef(owner: Option[Symbol],
      name: Name, pos: Option[Position]): Unit = owner.foreach { owner =>
    if(owner.directlyDefinesName(name,
            _.isInstanceOf[MethodSymbol]))
        error(METHOD_ALREADY_DEFINED,
            "", "", pos)
  }
}

@component
trait ValDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (valdef: ValDefApi)          => {
    val owner   = valdef.owner
    owner.foreach { o =>
      valdef.tpt.owner = o
    }
    val tpt     = assign(valdef.tpt).asInstanceOf[UseTree]
    val symbol  = VariableSymbol(valdef.mods, valdef.name,
      tpt.symbol, owner)
    valdef.rhs.owner = symbol
    val rhs     = assign(valdef.rhs).asInstanceOf[Expr]
    checkDoubleDef(owner, valdef.name, valdef.pos)
    declareSymbol(valdef, symbol)
    valdef.symbol = symbol
    TreeCopiers.copyValDef(valdef)(tpt = tpt, rhs = rhs)
  }


  protected def declareSymbol(valdef: ValDefApi,
    symbol: Symbol): Unit = {
   if(valdef.mods.isField) {
      valdef.owner.foreach(sym => {
        sym.declare(symbol)
      })
    }
  }

  protected def checkDoubleDef(owner: Option[Symbol],
      name: Name, pos: Option[Position]): Unit =
    if(owner.map(_.directlyDefinesName(name,
        _.isInstanceOf[VariableSymbol])).getOrElse(false))
      error(VARIABLE_ALREADY_DEFINED,
          "", "", pos)
}


@component
trait TypeUseSymbolAssignerComponent extends SymbolAssignerComponent {
  (tuse: TypeUseApi)          => {
    tuse
  }
}

@component
trait ForSymbolAssignerComponent extends SymbolAssignerComponent {
  (forloop: ForApi)          => {
    val owner   = forloop.owner
    val symbol  = ScopeSymbol(owner)
    val inits = forloop.inits.map { init =>
      init.owner = symbol
      assign(init)
    }
    forloop.cond.owner  = symbol
    forloop.body.owner = symbol
    val cond = assign(forloop.cond).asInstanceOf[Expr]
    val steps = forloop.steps.map { step =>
      step.owner = symbol
      assign(step).asInstanceOf[Expr]
    }
    val body = assign(forloop.body).asInstanceOf[Expr]
    forloop.symbol = symbol
    TreeCopiers.copyFor(forloop)(inits = inits, cond = cond,
      steps = steps, body = body)
  }
}

@component
trait BlockSymbolAssignerComponent extends SymbolAssignerComponent {
  (block: BlockApi)          => {
    val owner   = block.owner
    val symbol  = ScopeSymbol(owner)
    block.symbol = symbol
    val stmts = block.stmts.map { stmt =>
      stmt.owner = symbol
      assign(stmt)
    }
    TreeCopiers.copyBlock(block)(stmts = stmts)
  }
}

// Boring cases, just pass the owner around and assign it to
// all the trees that can have an owner
@component
trait IdentSymbolAssignerComponent extends SymbolAssignerComponent {
  (id: IdentApi)          => id
}

@component
trait BinarySymbolAssignerComponent extends SymbolAssignerComponent {
  (bin: BinaryApi)          => {
    val owner = bin.owner
    owner.foreach { o =>
      bin.lhs.owner = o
      bin.rhs.owner = o
    }
    val lhs = assign(bin.lhs).asInstanceOf[Expr]
    val rhs = assign(bin.rhs).asInstanceOf[Expr]
    TreeCopiers.copyBinary(bin)(lhs = lhs, rhs = rhs)
  }
}

@component
trait UnarySymbolAssignerComponent extends SymbolAssignerComponent {
  (unary: UnaryApi)          => {
    val owner = unary.owner
    owner.foreach { o =>
      unary.expr.owner = o
    }
    val expr = assign(unary.expr).asInstanceOf[Expr]
    TreeCopiers.copyUnary(unary)(expr = expr)
  }
}

@component
trait CastSymbolAssignerComponent extends SymbolAssignerComponent {
  (cast: CastApi)          => {
    val owner = cast.owner
    owner.foreach { o =>
      cast.expr.owner = o
      cast.tpt.owner  = o
    }
    val tpt  = assign(cast.tpt).asInstanceOf[UseTree]
    val expr = assign(cast.expr).asInstanceOf[Expr]
    TreeCopiers.copyCast(cast)(tpt = tpt, expr = expr)
  }
}

@component
trait ReturnSymbolAssignerComponent extends SymbolAssignerComponent {
  (ret: ReturnApi)          => {
    val owner = ret.owner
    owner.foreach { o =>
      ret.expr.foreach(_.owner = o)
    }
    val expr = ret.expr.map( x =>
      assign(x).asInstanceOf[Expr])
    TreeCopiers.copyReturn(ret)(expr = expr)
  }
}

@component
trait AssignSymbolAssignerComponent extends SymbolAssignerComponent {
  (assgn: AssignApi)          => {
    val owner = assgn.owner
    owner.foreach { o =>
      assgn.lhs.owner = o
      assgn.rhs.owner = o
    }
    val lhs = assign(assgn.lhs).asInstanceOf[Expr]
    val rhs = assign(assgn.rhs).asInstanceOf[Expr]
    TreeCopiers.copyAssign(assgn)(lhs = lhs, rhs = rhs)
  }
}


@component
trait TernarySymbolAssignerComponent extends SymbolAssignerComponent {
  (tern: TernaryApi)          => {
    val owner = tern.owner
    owner.foreach { o =>
      tern.cond.owner = o
      tern.thenp.owner = o
      tern.elsep.owner = o
    }
    val cond = assign(tern.cond).asInstanceOf[Expr]
    val thenp = assign(tern.thenp).asInstanceOf[Expr]
    val elsep = assign(tern.elsep).asInstanceOf[Expr]
    TreeCopiers.copyTernary(tern)(cond = cond,
      thenp = thenp, elsep = elsep)
  }
}

@component
trait IfSymbolAssignerComponent extends SymbolAssignerComponent {
  (ifelse: IfApi)          => {
    val owner = ifelse.owner
    owner.foreach { o =>
      ifelse.cond.owner = o
      ifelse.thenp.owner = o
      ifelse.elsep.owner = o
    }
    val cond = assign(ifelse.cond).asInstanceOf[Expr]
    val thenp = assign(ifelse.thenp).asInstanceOf[Expr]
    val elsep = assign(ifelse.elsep).asInstanceOf[Expr]
    TreeCopiers.copyIf(ifelse)(cond = cond, thenp = thenp,
      elsep = elsep)
  }
}

@component
trait WhileSymbolAssignerComponent extends SymbolAssignerComponent {
  (wile: WhileApi)          => {
    val owner = wile.owner
    owner.foreach { o =>
      wile.cond.owner = o
      wile.body.owner = o
    }
    val cond = assign(wile.cond).asInstanceOf[Expr]
    val body = assign(wile.body).asInstanceOf[Expr]
    TreeCopiers.copyWhile(wile)(cond = cond, body = body)
  }
}

@component
trait ApplySymbolAssignerComponent extends SymbolAssignerComponent {
  (apply: ApplyApi)          => {
    val owner = apply.owner
    owner.foreach { o =>
      apply.fun.owner = o
      apply.args.foreach(_.owner = o)
    }
    val fun = assign(apply.fun).asInstanceOf[Expr]
    val args = apply.args.map { arg =>
      assign(arg).asInstanceOf[Expr]
    }
    TreeCopiers.copyApply(apply)(fun = fun, args = args)
  }
}

@component
trait LiteralSymbolAssignerComponent extends SymbolAssignerComponent {
  (lit: LiteralApi) => lit
}
