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

package ch.usi.inf.l3.sana.ooj.namers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj

import tiny.core.TransformationComponent
import tiny.dsl._
import tiny.ast.{TreeCopiers => _, TreeFactories => _, _}
import tiny.errors.ErrorReporting.{error,warning}
import tiny.symbols._
import tiny.source.Position
import tiny.names.Name
import calcj.ast.{TreeCopiers => _, TreeFactories => _, _}
import primj.ast.{TreeCopiers => _, MethodDefApi => PMethodDefApi,
                  ProgramApi => _, TreeFactories => _, TreeUtils => _, _}
import ooj.errors.ErrorCodes._
import primj.symbols.{ProgramSymbol, MethodSymbol, VoidSymbol}
import primj.modifiers._
import ooj.names.StdNames
import ooj.modifiers._
import ooj.modifiers.Ops._
import primj.namers.SymbolAssignerComponent
import ooj.ast._
import ooj.ast.Implicits._
import ooj.ast.TreeExtractors._
import ooj.symbols._



/*
CompilationUnit: DONE
PackageDef: DONE
ClassDef: DONE
Template: DONE
New: DONE
Select: DONE
This: DONE
Super: DONE
MethodDef: DONE
*/

@component
trait ProgramSymbolAssignerComponent extends SymbolAssignerComponent {
  (prg: ProgramApi) => {
    val sym     = ProgramSymbol
    val members = prg.members.map { x =>
        x.owner = sym
        assign(x)
      }
    TreeCopiers.copyProgram(prg)(members = members)
  }
}

@component
trait CompilationUnitSymbolAssignerComponent extends SymbolAssignerComponent {
  (cunit: CompilationUnitApi) => {
    // val owner   = cunit.owner
    // val sym     = CompilationUnitSymbol(None, cunit.sourceName,
    //                               cunit.sourcePath, owner)
    // owner.foreach(owner => {
    //   owner.declare(sym)
    // })
    // cunit.module.owner = sym
    // cunit.symbol       = sym
    // val pkg            = assign(cunit.module).asInstanceOf[PackageDefApi]
    // sym.module         = pkg.symbol
    // sym.owner.foreach(cunit.owner = _)
    // TreeCopiers.copyCompilationUnit(cunit)(module = pkg)

    val sym     = CompilationUnitSymbol(None, cunit.sourceName,
                                  cunit.sourcePath, None)
    cunit.module.owner = sym
    val pkg     = assign(cunit.module).asInstanceOf[PackageDefApi]
    val owner   = pkg.symbol
    sym.owner   = owner
    sym.module       = Some(sym)
    owner.foreach(owner => {
      cunit.owner = owner
      owner.declare(sym)
    })
    cunit.symbol       = sym
    TreeCopiers.copyCompilationUnit(cunit)(module = pkg)

  }
}

@component
trait PackageDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (pkg: PackageDefApi) => {
    val owner = pkg.owner
    val sym     = createSymbol(pkg.containingPackages,
      pkg.name, Some(programSymbol))
    // sym.owner.foreach(pkg.owner = _)
    pkg.symbol = sym
    val memberOwner = owner match {
      case Some(cunit: CompilationUnitSymbol) =>
        cunit.owner = Some(sym)
        cunit
      case _                                  =>
        sym
    }
    val members = pkg.members.map { member =>
      member.owner = memberOwner
      assign(member)
    }
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }


  protected def createSymbol(names: List[Name], name: Name,
    owner: Option[Symbol]): Symbol = {
    names match {
      case (n::ns) if name != StdNames.DEFAULT_PACKAGE_NAME      =>
        val s   = createSymbol(Nil, n, owner)
        createSymbol(ns, name, Some(s))
      case _                                                     =>
        val res = owner.flatMap(_.getDirectlyDefinedSymbol(name,
            _.isInstanceOf[PackageSymbol])) match {
          case None               =>
            val sym = PackageSymbol(name, owner)
            owner.foreach(_.declare(sym))
            sym
          case Some(sym)          =>
            sym
        }
        res
    }
  }

  protected def programSymbol: Symbol = ProgramSymbol
}

@component
trait ClassDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (clazz: ClassDefApi) => {
    val owner = clazz.owner
    val parents  = clazz.parents.map { parent =>
      owner.foreach(parent.owner = _)
      assign(parent).asInstanceOf[UseTree]
    }
    val sym = createClassSymbol(clazz, owner)
    clazz.symbol = sym
    classDoubleDefCheck(owner, clazz.name, clazz.pos)
    owner.foreach(_.declare(sym))
    val template = {
      val temp = addDefaultConstructor(clazz, sym)
      temp.owner        = sym
      val res = assign(temp).asInstanceOf[TemplateApi]
      res
    }
    TreeCopiers.copyClassDef(clazz)(parents = parents, body = template)
  }


  protected def createClassSymbol(clazz: ClassDefApi,
    owner: Option[Symbol]): ClassSymbol = owner match {
      case Some(cunit: CompilationUnitSymbol)                             =>
            // if clazz.mods.isPublicAcc                                     =>
        val s = ClassSymbol(clazz.mods, clazz.name, Nil, owner, None)
        cunit.owner.foreach(_.declare(s))
        s
      case _                                                              =>
        ClassSymbol(clazz.mods, clazz.name, Nil, owner, None)
    }

  protected def classDoubleDefCheck(owner: Option[Symbol],
    name: Name, pos: Option[Position]): Unit = owner match {
    case Some(owner) if owner.directlyDefinesName(name,
                               _.isInstanceOf[ClassSymbol])     =>
      error(CLASS_ALREADY_DEFINED,
          name.asString, "", pos)
    case _                                                      =>
      ()
  }

  protected def addDefaultConstructor(clazz: ClassDefApi, sym: ClassSymbol ): TemplateApi = {
      val shouldCreateConstructor =
        !(clazz.body.members.exists(isConstructor(_))) &&
          ! clazz.mods.isInterface
      shouldCreateConstructor match {
        case false               =>
          clazz.body
        case true                =>
          // TODO: Do we need to have a refactoring for creating constructors?
          // I would say yes!!!
          // INFO: No constructors? Add it!
          val mods          = {
            if(clazz.mods.isPublicAcc)         CONSTRUCTOR | PUBLIC_ACC
            else                               CONSTRUCTOR | PACKAGE_ACC
          }
          val body          = if(isObjectClass(sym)){
            TreeFactories.mkBlock(Nil, clazz.pos)
          } else {
             val spr          = TreeFactories.mkSuper(clazz.pos)
            val id            =
              TreeFactories.mkIdent(constructorName, clazz.pos)
            id.isConstructorIdent = true
            val slct          = TreeFactories.mkSelect(spr, id, clazz.pos)
            val app           = TreeFactories.mkApply(slct, Nil, clazz.pos)
            TreeFactories.mkBlock(List(app), clazz.pos)
          }
          val ret           = TreeFactories.mkTypeUse(clazz.name, clazz.pos)
          val const         = TreeFactories.mkMethodDef(mods, ret,
            constructorName, Nil, body, clazz.pos)
          TreeCopiers.copyTemplate(clazz.body)(members =
            const::clazz.body.members)
      }
    }

  protected def constructorName: Name       = StdNames.CONSTRUCTOR_NAME
  protected def voidSymbol: Option[Symbol]  = Some(VoidSymbol)

  protected def isConstructor(tree: Tree): Boolean =
    TreeUtils.isConstructor(tree)
  protected def enclosingPackage(sym: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingPackage(sym)

  protected def langPackageSymbol: Symbol = SymbolUtils.langPackageSymbol

  protected def isObjectClass(sym: Symbol): Boolean = {
    sym.name == StdNames.OBJECT_TYPE_NAME &&
        enclosingPackage(sym.owner) == Some(langPackageSymbol)
  }
}


@component
trait BlockSymbolAssignerComponent extends
  primj.namers.BlockSymbolAssignerComponent {
  (block: BlockApi)          => {
    val res = super.apply(block).asInstanceOf[BlockApi]
    block.owner match {
      case Some(_: ClassSymbol)   =>
        res.isStaticInit = true
        res.symbol.foreach(s => s.mods = s.mods | STATIC_INIT | STATIC)
      case _                      =>
        ()
    }
    res
  }
}

@component
trait TemplateSymbolAssignerComponent extends SymbolAssignerComponent {
  (tmpl: TemplateApi) => {
    val owner    = tmpl.owner
    val members  = tmpl.members.map { member =>
      owner.foreach(member.owner = _)
      assign(member)
    }
    TreeCopiers.copyTemplate(tmpl)(members = members)
  }
}

@component
trait NewSymbolAssignerComponent extends SymbolAssignerComponent {
  (nu: NewApi) => {
    nu.owner.foreach(nu.app.owner = _)
    val app      = assign(nu.app).asInstanceOf[ApplyApi]
    TreeCopiers.copyNew(nu)(app = app)
  }
}

@component
trait SelectSymbolAssignerComponent extends SymbolAssignerComponent {
  (select: SelectApi) => {
    val owner = select.owner
    owner.foreach(select.qual.owner = _)
    val qual           = assign(select.qual)
    val slctdOwner     = qual.symbol
    val slctdEnclosing = owner
    slctdEnclosing.foreach(select.tree.enclosing = _)
    select.tree.isQualified = true
    slctdOwner.foreach(select.tree.owner = _)
    val tree           =
      assign(select.tree).asInstanceOf[SimpleUseTree]
    TreeCopiers.copySelect(select)(tree = tree, qual = qual)
  }
}

@component
trait ThisSymbolAssignerComponent extends SymbolAssignerComponent {
  (ths: ThisApi) => {
    val owner = ths.owner
    val encl = enclosingClass(owner)
    encl.foreach(ths.enclosingClassSymbol = _)
    ths
  }

  def enclosingClass(owner: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(owner)
}

@component
trait SuperSymbolAssignerComponent extends SymbolAssignerComponent {
  (spr: SuperApi) => {
    val owner = spr.owner
    val enclosingClass = SymbolUtils.enclosingClass(owner)
    enclosingClass.foreach(spr.enclosingClassSymbol = _)
    spr
  }
}

@component
trait MethodDefSymbolAssignerComponent
    extends SymbolAssignerComponent {
  (mthd: PMethodDefApi)          => {
    mthd match {
      case mthd: MethodDefApi                   =>
        val owner   = mthd.owner
        val symbol  = createMethodSymbol(mthd, owner)
        owner.foreach(sym => sym.declare(symbol))
        val tpt     = if(mthd.mods.isConstructor) {
          mthd.declaredClassNameForConstructor = useName(mthd.ret)
          val tuse = TreeFactories.mkTypeUse(voidName, mthd.ret.pos)
          owner.foreach(tuse.owner = _)
          assign(tuse).asInstanceOf[UseTree]
        } else {
          owner.foreach(mthd.ret.owner = _)
          assign(mthd.ret).asInstanceOf[UseTree]
        }
        val mods = owner match {
          case Some(sym) if sym.mods.isInterface =>
            PUBLIC_ACC | mthd.mods
          case _                                 =>
            mthd.mods
        }
        val params  = mthd.params.map { param =>
          val p = if(!param.mods.isParam) {
            val mods = param.mods | PARAM
            TreeCopiers.copyValDef(param)(mods = mods)
          } else param
          p.owner = symbol
          assign(p).asInstanceOf[ValDefApi]
        }
        val body    = if(mthd.mods.isConstructor) {
          mthd.body match {
            case Block(List(Apply(Select(_: ThisApi,
                        Ident(`constructorName`)), _), _*)) |
                 Block(List(Apply(Select(_: SuperApi,
                        Ident(`constructorName`)), _), _*))               =>
              mthd.body.owner = symbol
              assign(mthd.body).asInstanceOf[Expr]
            case body: BlockApi           if !isObjectClass(mthd.owner)   =>
              val newBody = TreeCopiers.copyBlock(body)(
                stmts = constructorCall(body.pos)::body.stmts)
              newBody.owner = symbol
              assign(newBody).asInstanceOf[Expr]
            case expr                     if !isObjectClass(mthd.owner)   =>
              val newBody = TreeFactories.mkBlock(
                List(constructorCall(expr.pos), expr), expr.pos)
              newBody.owner = symbol
              assign(newBody).asInstanceOf[Expr]
            case expr                                                     =>
              val newBody = TreeFactories.mkBlock(
                List(expr), expr.pos)
              newBody.owner = symbol
              assign(newBody).asInstanceOf[Expr]
          }
        } else {
          mthd.body.owner = symbol
          assign(mthd.body).asInstanceOf[Expr]
        }
        symbol.params = params.flatMap(_.symbol)
        // symbol.ret    = tpt.symbol
        symbol.mods = mods
        mthd.symbol = symbol
        TreeCopiers.copyMethodDef(mthd)(mods = mods, ret = tpt,
          params = params, body = body)
      case mthd: PMethodDefApi                   =>
        val res = TreeUpgraders.upgradeMethodDef(mthd)
        assign(res)
    }
  }

  protected def createMethodSymbol(mthd: MethodDefApi,
    owner: Option[Symbol]): MethodSymbol =
    MethodSymbol(mthd.mods, mthd.name, None, Nil, None, owner)

  protected def constructorCall(pos: Option[Position]): Expr = {
    val id = TreeFactories.mkIdent(constructorName, pos)
    id.isConstructorIdent = true
    TreeFactories.mkApply(
      TreeFactories.mkSelect(
        TreeFactories.mkSuper(pos),
          id, pos), Nil, pos)
  }


  protected def isObjectClass(owner: Option[Symbol]): Boolean = {
    val res = owner.map(sym => sym.name == StdNames.OBJECT_TYPE_NAME &&
              enclosingPackage(sym.owner) == Some(langPackageSymbol) )
    res.getOrElse(false)
  }

  protected def useName(use: UseTree): Name = use match {
    case id: IdentApi      => id.name
    case tuse: TypeUseApi  => tuse.name
    case Select(_, i)      => i.name
  }

  protected def voidName: Name              = StdNames.VOID_TYPE_NAME
  protected val constructorName: Name       = StdNames.CONSTRUCTOR_NAME
  protected val langPackageSymbol: Symbol   = SymbolUtils.langPackageSymbol
  protected def enclosingPackage(sym: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingPackage(sym)
}


@component
trait ValDefSymbolAssignerComponent
  extends primj.namers.ValDefSymbolAssignerComponent {
  (valdef: ValDefApi) => {
    val owner = valdef.owner
    owner match {
      case Some(sym) if sym.mods.isInterface =>
        val mods = PUBLIC_ACC | STATIC | FINAL | FIELD | valdef.mods
        val nv   = TreeCopiers.copyValDef(valdef)(mods = mods)
        owner.foreach(nv.owner = _)
        super.apply(nv)
      case Some(sym) if sym.mods.isClass     =>
        val mods = FIELD | valdef.mods
        val nv   = TreeCopiers.copyValDef(valdef)(mods = mods)
        owner.foreach(nv.owner = _)
        super.apply(nv)
      case _                                 =>
        super.apply(valdef)
    }
  }

  protected def enclosingClass(symbol: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(symbol)
}


// @component
// trait IdentSymbolAssignerComponent extends SymbolAssignerComponent {
//   (id: Ident)          => {
//     owner.foreach(id.owner = _)
//     id
//   }
// }
//
// @component
// trait TypeUseSymbolAssignerComponent extends SymbolAssignerComponent {
//   (tuse: TypeUse)          => {
//     val symbol = owner.flatMap(_.getSymbol(tuse.name,
//       _.isInstanceOf[TypeSymbol]))
//     symbol match {
//       case Some(sym)      =>
//         owner.foreach(tuse.owner = _)
//         tuse
//       case _              => tuse
//     }
//   }
// }
