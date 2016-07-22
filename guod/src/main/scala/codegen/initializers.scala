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

package ch.usi.inf.l3.sana.guod.codegen



import ch.usi.inf.l3.sana
import sana.guod
import sana.tiny
import sana.ooj
import sana.primj
import sana.calcj



import tiny.dsl._
import tiny.core.TransformationComponent
import tiny.modifiers.Flags
import tiny.names.Name
import ooj.modifiers.{STATIC, STATIC_INIT}
import guod.modifiers.ModifiersUtils
import guod.ast.TreeFactories
import guod.ast.TreeExtractors._
import guod.ast._
import tiny.symbols.Symbol
import calcj.types.{LongType, DoubleType}
import guod.names.StdNames
import guod.symbols.SymbolUtils
import guod.ast.Implicits._
import ooj.modifiers.Ops._

/**
 * This phase lowers the AST by eliminating all static initializers and
 * introducing a `clinit`. Then it move all the field initializers that are not
 * static and literal to their respective initializer methods (namely either
 * constructors or clinit). Initializers will not be added to the constructors
 * that explicitly invoke another local constructor (not super constructor),
 * this way we avoid accidentally initializing a field twice.
 */
trait InitializerComponent extends
  TransformationComponent[Tree, Tree] {
  def inline: Tree => Tree
}

// Program: DONE
// PackageDef: DONE
// ClassDef: DONE
// Template: DONE
// CompilationUnit: DONE


@component
trait ProgramInitializerComponent extends InitializerComponent {
  (program: ProgramApi) => {
    val members = program.members.map(m => inline(m))
    TreeCopiers.copyProgram(program)(members = members)
  }
}

@component
trait PackageDefInitializerComponent extends InitializerComponent {
  (pkg: PackageDefApi) => {
    val members = pkg.members.map(inline(_))
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }
}

@component
trait CompilationUnitInitializerComponent extends InitializerComponent {
  (cunit: CompilationUnitApi) => {
    val module = inline(cunit.module).asInstanceOf[PackageDefApi]
    TreeCopiers.copyCompilationUnit(cunit)(module = module)
  }
}

@component
trait ClassDefInitializerComponent extends InitializerComponent {
  (clazz: ClassDefApi) => {
    val body = inline(clazz.body).asInstanceOf[TemplateApi]
    TreeCopiers.copyClassDef(clazz)(body = body)
  }
}

@component
trait TemplateInitializerComponent extends InitializerComponent {
  (template: TemplateApi) => {
    val z1: List[Tree] = Nil
    val z2: List[Tree] = Nil
    val (staticInits, instanceInits) = template.members.foldLeft((z1, z2)) {
      (z, y) => {
        y match {
          case valdef: ValDefApi        if valdef.rhs != NoTree          =>
            val qual = if(valdef.mods.isStatic) {
              val sym = enclosingClass(valdef.owner)
              val name = sym.map(_.name).getOrElse(StdNames.noname)
              TreeFactories.mkTypeUse(name, valdef.pos)
            } else {
              TreeFactories.mkThis(valdef.pos)
            }
            val id = TreeFactories.mkIdent(valdef.name, valdef.pos)
            val select = TreeFactories.mkSelect(qual, id, valdef.pos)
            val assign = TreeFactories.mkAssign(select, valdef.rhs, valdef.pos)
            // val res = compiler.typeCheck(valdef.owner)(assign)
            valdef.symbol.foreach(id.symbol = _)
            valdef.tpe.foreach { tpe =>
              id.tpe = tpe
              assign.tpe = tpe
            }
            valdef.owner.foreach { owner =>
              id.owner = owner
              assign.owner = owner
            }
            val p      = valdef.rhs match {
              case Literal(NullConstant)     =>
                false
              case _: LiteralApi             =>
                true
              case _                         =>
                false
            }
            if(valdef.mods.isStatic && p)
              (z._1 ++ List(assign), z._2)
            else if(!valdef.mods.isStatic)
              (z._1, z._2 ++ List(assign))
            else z
          case block: BlockApi                                           =>
            (z._1 ++ block.stmts, z._2)
          case _                                                         =>
            z
        }
      }
    }


    val members    = template.members.filter(!_.isInstanceOf[BlockApi]).map {
      case mthd: MethodDefApi      if mthd.mods.isConstructor =>
        mthd.body match {
          case block: BlockApi              =>
            val body = block.stmts match {
              case (hd@Apply(Select(_: ThisApi, _), _))::rest       =>
                block
              case (hd@Apply(Select(_: SuperApi, _), _))::rest      =>
                val stmts = hd::(instanceInits ++ rest)
                TreeCopiers.copyBlock(block)(stmts = stmts)
              case _                                                =>
                block
            }
            TreeCopiers.copyMethodDef(mthd)(body = body)
          case _                             =>
            mthd
        }
      case member                                             =>
          member
    }

    val clinit = {
      val mods   = STATIC | STATIC_INIT
      val ret    = TreeFactories.mkTypeUse(Name("void"), template.pos)
      val name   = StdNames.CLINIT_NAME
      val body1  = TreeFactories.mkBlock(staticInits, template.pos)
      val body2  = TreeFactories.mkBlock(Nil, template.pos)
      val res    = compiler.typeCheck(template.owner) {
        TreeFactories.mkMethodDef(mods, ret, name, Nil,
          Nil, body1, template.pos)
      }.asInstanceOf[MethodDefApi]
      TreeCopiers.copyMethodDef(res)(body = body2)
    }

    TreeCopiers.copyTemplate(template)(members = members ++ List(clinit))

  }

  /** @see [[SymbolUtils.enclosingClass]] */
  protected def enclosingClass(owner: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(owner)
}
