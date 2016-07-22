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

package ch.usi.inf.l3.sana.oberon0.typechecker

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
import tiny.ast.{DefTree, UseTree, Expr, Tree}
import tiny.types._
import calcj.types.IntType
import tiny.names.StdNames._
import tiny.names.Name
import ooj.types.RefType
import tiny.source.Position
import primj.modifiers.Ops._
import tiny.symbols.{Symbol, TypeSymbol}
import ooj.symbols.ClassSymbol
import ooj.ast.{ClassDefApi, TemplateApi}
import calcj.ast.{UnaryApi, LiteralApi, BinaryApi}
import tiny.errors.ErrorReporting.{error, warning}
import oberon0.errors.ErrorCodes._
import primj.ast.{BlockApi, ValDefApi, IfApi, AssignApi, ApplyApi, WhileApi}
import arrayj.ast.ArrayAccessApi
import oberon0.ast.Implicits._
import oberon0.symbols._
import oberon0.types._
import calcj.typechecker.TyperComponent
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
trait ModuleDefTyperComponent extends TyperComponent {
  (module: ModuleDefApi) => {
    val declarations = module.declarations.map(typed(_).asInstanceOf[DefTree])
    val block        = module.block.map(typed(_).asInstanceOf[BlockApi])
    TreeCopiers.copyModuleDef(module)(declarations =
      declarations, block = block)
  }
}

@component
trait ClassDefTyperComponent extends TyperComponent {
  (clazz: ClassDefApi) => {
    val body         = typed(clazz.body).asInstanceOf[TemplateApi]
    TreeCopiers.copyRecordDef(clazz)(body = body)
  }
}



@component
trait ValDefTyperComponent extends primj.typechecker.ValDefTyperComponent {
  (valdef: ValDefApi)    => {
    val rhs    = typed(valdef.rhs).asInstanceOf[Expr]
    val tpt = if(valdef.mods.isFinal) {
                val res = typeToTypeUse(rhs.tpe)
                valdef.pos.foreach(res.pos = _)
                compiler.typeCheck(valdef.symbol)(res).asInstanceOf[UseTree]
              } else typed(valdef.tpt).asInstanceOf[UseTree]
    val ttpe   = tpt.tpe.getOrElse(ErrorType)
    setTypeSymbol(valdef, tpt)
    valdef.tpe = ttpe
    val res = TreeCopiers.copyValDef(valdef)(tpt = tpt, rhs = rhs)
    checkValDef(res)
    res
  }

  /**
   * Given a type, returns a type use around it
   *
   * @param tpe the type to return a type use for it
   */
  protected def typeToTypeUse(tpe: Option[Type]): UseTree = tpe match {
    case Some(t: ArrayType)      =>
      TreeFactories.mkArrayTypeUse(typeToTypeUse(tpe), t.size)
    case Some(t: RefType)        =>
      TreeFactories.mkTypeUse(t.name)
    case Some(t: Type)           =>
      TreeFactories.mkTypeUse(Name(tpeToString(tpe)))
    case _                       =>
      TreeFactories.mkTypeUse(noname)
  }
}


@component
trait TypeDefTyperComponent extends TyperComponent {
  (tdef: TypeDefApi) => {
    val tpt         = typed(tdef.tpt)
    tpt.tpe.foreach(tdef.tpe = _)
    TreeCopiers.copyTypeDef(tdef)(tpt = tpt)
  }
}


@component
trait ArrayTypeUseTyperComponent extends TyperComponent {
  (tuse: ArrayTypeUseApi) => {
    val tpt           = typed(tuse.tpt).asInstanceOf[UseTree]
    val size          = typed(tuse.size).asInstanceOf[Expr]
    val stpe          = size.tpe.getOrElse(ErrorType)
    stpe match {
      case IntType           => ()
      case t                 =>
        error(TYPE_MISMATCH, tpeToString(Some(t)), IntType.toString,
          tuse.pos)
    }
    tpt.tpe.foreach(tpe => tuse.tpe = ArrayType(tpe, size))
    TreeCopiers.copyArrayTypeUse(tuse)(tpt = tpt, size = size)
  }
}
