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

package ch.usi.inf.l3.sana.oberon0.namers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.arrayj
import sana.ooj
import sana.oberon0

import tiny.dsl._
import oberon0.ast._
import tiny.ast.{IdentApi, DefTree, UseTree, Expr, Tree, SimpleUseTree}
import tiny.names.Name
import tiny.source.Position
import tiny.symbols.{Symbol, TypeSymbol}
import ooj.symbols.ClassSymbol
import ooj.ast.{ClassDefApi, TemplateApi, SelectApi}
import calcj.ast.{UnaryApi, LiteralApi, BinaryApi}
import tiny.errors.ErrorReporting.{error, warning}
import oberon0.errors.ErrorCodes._
import primj.ast.{MethodDefApi, BlockApi, ValDefApi, IfApi, AssignApi, ApplyApi, WhileApi}
import arrayj.ast.ArrayAccessApi
import oberon0.ast.Implicits._
import primj.symbols.VariableSymbol
import oberon0.symbols._
import oberon0.types._
import primj.namers.NamerComponent
import oberon0.symbols.SymbolUtils


// Program: DONE
// ModuleDef: DONE
// ClassDef: DONE
// Template: DONE
// TypeDef: DONE
// ArrayTypeUse: DONE
// TypeUse: DONE
// Ident: DONE
// MethodDef: DONE
// ValDef: DONE
// Apply: DONE
// If: DONE
// While: DONE
// Block: DONE
// Assign: DONE
// ArrayAccess: DONE
// Select: DONE
// Binary: DONE
// Unary: DONE
// Literal: DONE



@component
trait ModuleDefNamerComponent extends NamerComponent {
  (module: ModuleDefApi) => {
    val declarations = module.declarations.map(name(_).asInstanceOf[DefTree])
    val block        = module.block.map(name(_).asInstanceOf[BlockApi])
    TreeCopiers.copyModuleDef(module)(declarations =
      declarations, block = block)
  }
}

@component
trait TypeDefNamerComponent extends NamerComponent {
  (tdef: TypeDefApi) => {
    val tpt = name(tdef.tpt)
    tdef.symbol.foreach {
      case sym: TypeDefSymbol => sym.typeSymbol = tpt.symbol
      case _                  => ()
    }
    TreeCopiers.copyTypeDef(tdef)(tpt = tpt)
  }
}

@component
trait ClassDefNamerComponent extends NamerComponent {
  (clazz: ClassDefApi) => {
    val body    = name(clazz.body).asInstanceOf[TemplateApi]
    val fields  = body.members.flatMap {
      case v: ValDefApi     =>
        v.tpe.map((v.name, _))
      case _                => None
    }
    val tpe     = RecordType(fields.toMap)
    clazz.tpe = tpe
    clazz.symbol.foreach(_.tpe = Some(tpe))
    TreeCopiers.copyRecordDef(clazz)(body = body)
  }
}

@component
trait ArrayTypeUseNamerComponent extends NamerComponent {
  (tuse: ArrayTypeUseApi) => {
    val tpt  = name(tuse.tpt).asInstanceOf[UseTree]
    val size = name(tuse.size).asInstanceOf[Expr]
    tpt.tpe.map { tpe =>
      tuse.tpe = ArrayType(tpe, size)
    }
    tpt.symbol.foreach { sym =>
      tuse.symbol = ArraySymbol(sym, size)
    }
    TreeCopiers.copyArrayTypeUse(tuse)(tpt = tpt, size = size)
  }
}

@component
trait BlockNamerComponent extends NamerComponent {
  (block: BlockApi) => {
    val stmts  = block.stmts.map(name(_))
    TreeCopiers.copyBlock(block)(stmts = stmts)
  }
}

@component
trait MethodDefNamerComponent extends primj.namers.MethodDefNamerComponent {
  (mthd: MethodDefApi)          => {
    val mthd2 = super.apply(mthd).asInstanceOf[MethodDefApi]
    val body = name(mthd2.body).asInstanceOf[Expr]
    TreeCopiers.copyMethodDef(mthd2)(body = body)
  }
}
@component
trait BinaryNamerComponent extends NamerComponent {
  (bin: BinaryApi)          => {
    val lhs = name(bin.lhs).asInstanceOf[Expr]
    val rhs = name(bin.rhs).asInstanceOf[Expr]
    TreeCopiers.copyBinary(bin)(lhs = lhs, rhs = rhs)
  }

}

@component
trait UnaryNamerComponent extends NamerComponent {
  (unary: UnaryApi)          => {
    val expr = name(unary.expr).asInstanceOf[Expr]
    TreeCopiers.copyUnary(unary)(expr = expr)
  }

}

@component
trait AssignNamerComponent extends NamerComponent {
  (assgn: AssignApi)          => {
    val lhs = name(assgn.lhs).asInstanceOf[Expr]
    val rhs = name(assgn.rhs).asInstanceOf[Expr]
    TreeCopiers.copyAssign(assgn)(lhs = lhs, rhs = rhs)
  }

}


@component
trait IfNamerComponent extends NamerComponent {
  (ifelse: IfApi)          => {
    val cond = name(ifelse.cond).asInstanceOf[Expr]
    val thenp = name(ifelse.thenp).asInstanceOf[Expr]
    val elsep = name(ifelse.elsep).asInstanceOf[Expr]
    TreeCopiers.copyIf(ifelse)(cond = cond, thenp = thenp,
      elsep = elsep)
  }

}

@component
trait WhileNamerComponent extends NamerComponent {
  (wile: WhileApi)          => {
    val cond = name(wile.cond).asInstanceOf[Expr]
    val body = name(wile.body).asInstanceOf[Expr]
    TreeCopiers.copyWhile(wile)(cond = cond, body = body)
  }

}

@component
trait ApplyNamerComponent extends NamerComponent {
  (apply: ApplyApi)          => {
    val fun = name(apply.fun).asInstanceOf[Expr]
    val args = apply.args.map { arg =>
      name(arg).asInstanceOf[Expr]
    }
    TreeCopiers.copyApply(apply)(fun = fun, args = args)
  }
}

@component
trait LiteralNamerComponent extends NamerComponent {
  (lit: LiteralApi)          => lit
}


@component
trait ArrayAccessNamerComponent extends NamerComponent {
  (access: ArrayAccessApi)          => {
    val array = name(access.array).asInstanceOf[Expr]
    val index = name(access.index).asInstanceOf[Expr]
    array.symbol match {
      case Some(vs: VariableSymbol) =>
        vs.typeSymbol.foreach(access.symbol = _)
        vs.tpe.foreach {
          case atpe: ArrayTypeApi => access.tpe = atpe.componentType
        }
      case _                        =>
        ()
    }
    TreeCopiers.copyArrayAccess(access)(array = array, index = index)
  }
}


@component
trait SelectNamerComponent extends NamerComponent {
  (select: SelectApi) => {
    val qual           = name(select.qual)
    val slctdOwner     = qual.symbol match {
      case Some(vsym: VariableSymbol) => vsym.typeSymbol
      case s                          => s
    }
    slctdOwner.foreach(select.tree.owner = _)
    val tree           = name(select.tree).asInstanceOf[SimpleUseTree]
    tree.symbol.foreach(select.symbol = _)
    TreeCopiers.copySelect(select)(qual = qual, tree = tree)
  }
}

@component
trait IdentNamerComponent extends primj.namers.IdentNamerComponent {
  (id: IdentApi)          => {
    val ident = super.apply(id).asInstanceOf[IdentApi]
    ident.hasBeenNamed = false
    ident
  }
}
