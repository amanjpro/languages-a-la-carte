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
import sana.modulej



import tiny.dsl._
import tiny.core.TransformationComponent
import tiny.modifiers.Flags
import tiny.names.Name
import ooj.modifiers.STATIC
import guod.modifiers.ModifiersUtils
import guod.ast.TreeExtractors._
import modulej.ast.TreeUtils
import guod.ast._
import tiny.symbols.Symbol
import calcj.types.{LongType, DoubleType}
import guod.names.StdNames
import guod.symbols.SymbolUtils
import guod.ast.Implicits._
import ooj.modifiers.Ops._

trait QualifierComponent extends
  TransformationComponent[Tree, Tree] {
  def fullyqualify: Tree => Tree
}

// Ident: DONE
// TypeUse:
// Cast: DONE
// Literal: DONE
// Unary: DONE
// Binary: DONE
// Block: DONE
// While: DONE
// If: DONE
// For: DONE
// Assign: DONE
// Apply: DONE
// ValDef: DONE
// Ternary: DONE
// Return: DONE
// Label: DONE
// Break: DONE
// Continue: DONE
// Case: DONE
// Switch: DONE
// ArrayInitializer: DONE
// ArrayTypeUse: DONE
// ArrayAccess: DONE
// ArrayCreation: DONE
// Program: DONE
// PackageDef: DONE
// ClassDef: DONE
// This: DONE
// Super: DONE
// Template: DONE
// New: DONE
// Select: DONE
// Try: DONE
// MethodDef: DONE
// Catch: DONE
// Throw: DONE
// Synchronized: DONE
// CompilationUnit: DONE



@component
trait ProgramQualifierComponent extends QualifierComponent {
  (program: ProgramApi) => {
    val members = program.members.map(m => fullyqualify(m))
    TreeCopiers.copyProgram(program)(members = members)
  }
}

@component
trait PackageDefQualifierComponent extends QualifierComponent {
  (pkg: PackageDefApi) => {
    val members = pkg.members.map(fullyqualify(_))
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }
}

@component
trait CompilationUnitQualifierComponent extends QualifierComponent {
  (cunit: CompilationUnitApi) => {
    val module = fullyqualify(cunit.module).asInstanceOf[PackageDefApi]
    TreeCopiers.copyCompilationUnit(cunit)(module = module)
  }
}

@component
trait ClassDefQualifierComponent extends QualifierComponent {
  (clazz: ClassDefApi) => {
    val parents = clazz.parents.map(fullyqualify(_).asInstanceOf[UseTree])
    val body    = fullyqualify(clazz.body).asInstanceOf[TemplateApi]
    TreeCopiers.copyClassDef(clazz)(body = body, parents = parents)
  }
}

@component
trait TemplateQualifierComponent extends QualifierComponent {
  (template: TemplateApi) => {
    val members = template.members.map(fullyqualify(_))
    TreeCopiers.copyTemplate(template)(members = members)

  }
}


@component
trait IdentQualifierComponent extends QualifierComponent {
  (ident: IdentApi) => {
    ident.symbol match {
      case Some(sym)    if !ident.isQualified &&
                           (sym.mods.isField || ident.isMethodIdent) &&
                           !sym.mods.isStatic                              =>
        val sym = enclosingClass(ident.owner)
        val ths = TreeFactories.mkThis(ident.pos)
        val res = TreeFactories.mkSelect(ths, ident, ident.pos)
        compiler.typeCheck(ident.owner)(res)
        // for {
        //   o <- ident.owner
        //   s <- sym
        //   t <- s.tpe
        // } {
        //   ths.tpe      = t
        //   ths.owner    = o
        //   ths.symbol   = s
        // }
        // ident.symbol.foreach(res.symbol = _)
        // ident.tpe.foreach(res.tpe = _)
        // res
      case Some(sym)    if !ident.isQualified &&
                           sym.mods.isStatic                               =>
        val sym = enclosingClass(ident.owner)
        val name = sym.map(_.name).getOrElse(StdNames.noname)
        val tuse = TreeFactories.mkTypeUse(name, ident.pos)
        // for {
        //   o <- ident.owner
        //   s <- sym
        //   t <- s.tpe
        // } {
        //   tuse.tpe      = t
        //   tuse.owner    = o
        //   tuse.symbol   = s
        // }
        val res = TreeFactories.mkSelect(tuse, ident, ident.pos)
        compiler.typeCheck(ident.owner)(res)
        // ident.symbol.foreach(res.symbol = _)
        // ident.tpe.foreach(res.tpe = _)
        // res
      case _                                                               =>
        ident
    }
  }

  protected def enclosingClass(owner: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(owner)
}


@component
trait SelectQualifierComponent extends QualifierComponent {
  (select: SelectApi) => {
    val qual = fullyqualify(select.qual)
    TreeCopiers.copySelect(select)(qual = qual)
  }
}

@component
trait NewQualifierComponent extends QualifierComponent {
  (nw: NewApi) => {
    val app = fullyqualify(nw.app).asInstanceOf[ApplyApi]
    TreeCopiers.copyNew(nw)(app = app)
  }
}

@component
trait ThrowQualifierComponent extends QualifierComponent {
  (thrw: ThrowApi) => {
    val expr = fullyqualify(thrw.expr).asInstanceOf[Expr]
    TreeCopiers.copyThrow(thrw)(expr = expr)
  }
}

@component
trait SynchronizedQualifierComponent extends QualifierComponent {
  (synch: SynchronizedApi) => {
    val block = fullyqualify(synch.block).asInstanceOf[BlockApi]
    TreeCopiers.copySynchronized(synch)(block = block)
  }
}

@component
trait LabelQualifierComponent extends QualifierComponent {
  (lbl: LabelApi) => {
    val stmt = fullyqualify(lbl.stmt).asInstanceOf[Expr]
    TreeCopiers.copyLabel(lbl)(stmt = stmt)
  }
}




@component
trait MethodDefQualifierComponent extends QualifierComponent {
  (mthd: MethodDefApi) => {
    val ret          = fullyqualify(mthd.ret).asInstanceOf[UseTree]
    val params       = mthd.params.map(fullyqualify(_).asInstanceOf[ValDefApi])
    val throwsClause =
      mthd.throwsClause.map(fullyqualify(_).asInstanceOf[UseTree])
    val body         = fullyqualify(mthd.body).asInstanceOf[Expr]
    TreeCopiers.copyMethodDef(mthd)(ret = ret, params = params,
      throwsClause = throwsClause, body = body)
  }
}

@component
trait ValDefQualifierComponent extends QualifierComponent {
  (valdef: ValDefApi) => {
    val tpt          = fullyqualify(valdef.tpt).asInstanceOf[UseTree]
    val rhs          = fullyqualify(valdef.rhs).asInstanceOf[Expr]
    TreeCopiers.copyValDef(valdef)(tpt = tpt, rhs = rhs)
  }
}

@component
trait TryQualifierComponent extends QualifierComponent {
  (tri: TryApi) => {
    val tryClause = fullyqualify(tri.tryClause).asInstanceOf[BlockApi]
    val catches   = tri.catches.map(fullyqualify(_).asInstanceOf[CatchApi])
    val finallyClause =
      tri.finallyClause.map(fullyqualify(_).asInstanceOf[BlockApi])
    TreeCopiers.copyTry(tri)(tryClause = tryClause, catches = catches,
      finallyClause = finallyClause)
  }
}


@component
trait BlockQualifierComponent extends QualifierComponent {
  (block: BlockApi) => {
    val stmts = block.stmts.map(fullyqualify(_))
    TreeCopiers.copyBlock(block)(stmts = stmts)
  }
}


@component
trait CastQualifierComponent extends QualifierComponent {
  (cast: CastApi) => {
    val tpt  = fullyqualify(cast.tpt).asInstanceOf[UseTree]
    val expr = fullyqualify(cast.expr).asInstanceOf[Expr]
    TreeCopiers.copyCast(cast)(tpt = tpt, expr = expr)
  }
}

@component
trait UnaryQualifierComponent extends QualifierComponent {
  (unary: UnaryApi) => {
    val expr = fullyqualify(unary.expr).asInstanceOf[Expr]
    TreeCopiers.copyUnary(unary)(expr = expr)
  }
}

@component
trait BinaryQualifierComponent extends QualifierComponent {
  (bin: BinaryApi) => {
    val lhs = fullyqualify(bin.lhs).asInstanceOf[Expr]
    val rhs = fullyqualify(bin.rhs).asInstanceOf[Expr]
    TreeCopiers.copyBinary(bin)(rhs = rhs, lhs = lhs)
  }
}

@component
trait ApplyQualifierComponent extends QualifierComponent {
  (app: ApplyApi) => {
    val fun  = fullyqualify(app.fun).asInstanceOf[Expr]
    val args = app.args.map(fullyqualify(_).asInstanceOf[Expr])
    TreeCopiers.copyApply(app)(fun = fun, args = args)
  }
}

@component
trait AssignQualifierComponent extends QualifierComponent {
  (assign: AssignApi) => {
    val lhs = fullyqualify(assign.lhs).asInstanceOf[Expr]
    val rhs = fullyqualify(assign.rhs).asInstanceOf[Expr]
    TreeCopiers.copyAssign(assign)(rhs = rhs, lhs = lhs)
  }
}


@component
trait TernaryQualifierComponent extends QualifierComponent {
  (tern: TernaryApi) => {
    val cond  = fullyqualify(tern.cond).asInstanceOf[Expr]
    val thenp = fullyqualify(tern.thenp).asInstanceOf[Expr]
    val elsep = fullyqualify(tern.elsep).asInstanceOf[Expr]
    TreeCopiers.copyTernary(tern)(cond = cond, thenp = thenp,
      elsep = elsep)
  }
}



@component
trait CatchQualifierComponent extends QualifierComponent {
  (ctch: CatchApi) => {
    val eparam = fullyqualify(ctch.eparam).asInstanceOf[ValDefApi]
    val body   = fullyqualify(ctch.catchClause).asInstanceOf[BlockApi]
    TreeCopiers.copyCatch(ctch)(eparam = eparam, catchClause = body)
  }
}


@component
trait ArrayAccessQualifierComponent extends QualifierComponent {
  (access: ArrayAccessApi) => {
    val array = fullyqualify(access.array).asInstanceOf[Expr]
    val index = fullyqualify(access.index).asInstanceOf[Expr]
    TreeCopiers.copyArrayAccess(access)(array = array, index = index)
  }
}


@component
trait ArrayInitializerQualifierComponent extends QualifierComponent {
  (array: ArrayInitializerApi) => {
    val inits = array.elements.map(fullyqualify(_).asInstanceOf[Expr])
    TreeCopiers.copyArrayInitializer(array)(elements = inits)
  }
}



@component
trait ReturnQualifierComponent extends QualifierComponent {
  (ret: ReturnApi) => {
    val expr = ret.expr.map(fullyqualify(_).asInstanceOf[Expr])
    TreeCopiers.copyReturn(ret)(expr = expr)
  }
}


@component
trait SwitchQualifierComponent extends QualifierComponent {
  (switch: SwitchApi) => {
    val expr  = fullyqualify(switch.expr).asInstanceOf[Expr]
    val cases = switch.cases.map(fullyqualify(_).asInstanceOf[CaseApi])
    TreeCopiers.copySwitch(switch)(expr = expr, cases = cases)
  }
}



@component
trait CaseQualifierComponent extends QualifierComponent {
  (cse: CaseApi) => {
    val body = fullyqualify(cse.body).asInstanceOf[Expr]
    TreeCopiers.copyCase(cse)(body = body)
  }
}


@component
trait WhileQualifierComponent extends QualifierComponent {
  (wile: WhileApi) => {
    val cond  = fullyqualify(wile.cond).asInstanceOf[Expr]
    val body  = fullyqualify(wile.body).asInstanceOf[Expr]
    TreeCopiers.copyWhile(wile)(cond = cond, body = body)
  }
}


@component
trait ForQualifierComponent extends QualifierComponent {
  (forloop: ForApi) => {
    val inits  = forloop.inits.map(fullyqualify(_))
    val cond   = fullyqualify(forloop.cond).asInstanceOf[Expr]
    val steps  = forloop.steps.map(fullyqualify(_).asInstanceOf[Expr])
    val body  = fullyqualify(forloop.body).asInstanceOf[Expr]
    TreeCopiers.copyFor(forloop)(steps = steps, inits = inits,
      cond = cond, body = body)
  }
}


@component
trait IfQualifierComponent extends QualifierComponent {
  (ifelse: IfApi) => {
    val cond  = fullyqualify(ifelse.cond).asInstanceOf[Expr]
    val thenp = fullyqualify(ifelse.thenp).asInstanceOf[Expr]
    val elsep = fullyqualify(ifelse.elsep).asInstanceOf[Expr]
    TreeCopiers.copyIf(ifelse)(cond = cond, thenp = thenp,
      elsep = elsep)
  }
}


@component
trait ArrayTypeUseQualifierComponent extends QualifierComponent {
  (atuse: ArrayTypeUseApi) => {
    val tpt = fullyqualify(atuse.tpt).asInstanceOf[UseTree]
    TreeCopiers.copyArrayTypeUse(atuse)(tpt = tpt)
  }
}


@component
trait ArrayCreationQualifierComponent extends QualifierComponent {
  (creation: ArrayCreationApi) => {
    val array  = fullyqualify(creation.array).asInstanceOf[Expr]
    val size   = creation.size.map(fullyqualify(_).asInstanceOf[Expr])
    TreeCopiers.copyArrayCreation(creation)(array = array, size = size)
  }
}


@component
trait TypeUseQualifierComponent extends QualifierComponent {
  (tuse: TypeUseApi) => {
    if(tuse.isQualified) tuse
    else {
      val fullName = toFullyQualifiedTypeName(tuse.symbol)
      if(fullName != tuse.name.asString) {
        val tree = fromQualifiedString(fullName) match {
          case s@Select(q, id: IdentApi)       =>
            val res = TreeFactories.mkTypeUse(id.name, tuse.pos)
            TreeFactories.mkSelect(q, res, tuse.pos)
          case id: IdentApi                    =>
            TreeFactories.mkTypeUse(id.name, tuse.pos)
          case t                               => t
        }
        compiler.typeCheck(tuse.owner)(tree)
      } else tuse
    }
  }

  protected def fromQualifiedString(name: String): UseTree =
    TreeUtils.fromQualifiedString(name)

  protected def toFullyQualifiedTypeName(symbol: Option[Symbol]): String =
    SymbolUtils.toFullyQualifiedTypeName(symbol)
}
