package ch.usi.inf.l3.sana.ooj.namers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast.{TreeCopiers => _, TreeFactories => _, _}
import tiny.errors.ErrorReporting.{error,warning}
import tiny.symbols._
import tiny.names.Name
import calcj.ast.{TreeCopiers => _, TreeFactories => _, _}
import primj.ast.{TreeCopiers => _, MethodDefApi => _,
                  TreeFactories => _, TreeUtils => _, _}
import primj.errors.ErrorCodes._
import primj.symbols.{MethodSymbol, VoidSymbol}
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

@component(tree, owner)
trait CompilationUnitSymbolAssignerComponent extends SymbolAssignerComponent {
  (cunit: CompilationUnitApi) => {
    val sym     = CompilationUnitSymbol(None, cunit.sourceName,
                                  cunit.sourcePath, owner)
    val pkg     = assign((cunit.module, Some(sym))).asInstanceOf[PackageDefApi]
    sym.module  = pkg.symbol
    owner.foreach(owner => {
      cunit.owner = owner
      owner.declare(sym)
    })
    TreeCopiers.copyCompilationUnit(cunit)(module = pkg)
  }
}

@component(tree, owner)
trait PackageDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (pkg: PackageDefApi) => {
    val sym     = PackageSymbol(pkg.name, owner)
    val members = pkg.members.map { member =>
      assign((member, Some(sym)))
    }
    pkg.symbol = sym
    owner.foreach(owner => {
      pkg.owner = owner
      owner.declare(sym)
    })
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }
}

@component(tree, owner)
trait ClassDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (clazz: ClassDefApi) => {
    val parents  = clazz.parents.map(parent =>
        assign((parent, owner)).asInstanceOf[UseTree])
    val sym      = ClassSymbol(clazz.mods, clazz.name,
      Nil, owner, None)
    val template = {
      val shouldCreateConstructor =
        !(clazz.body.members.exists(isConstructor(_))) &&
          ! clazz.mods.isInterface
      shouldCreateConstructor match {
        case false               =>
          assign((clazz.body, Some(sym))).asInstanceOf[TemplateApi]
        case true                =>
          // TODO: Do we need to have a refactoring for creating constructors?
          // I would say yes!!!
          // INFO: No constructors? Add it!
          val mods          = {
            if(clazz.mods.isPublicAcc)         CONSTRUCTOR | PUBLIC_ACC
            else                               CONSTRUCTOR | PACKAGE_ACC
          }
          val spr           = TreeFactories.mkSuper(clazz.pos)
          val id            = TreeFactories.mkIdent(constructorName, clazz.pos)
          val slct          = TreeFactories.mkSelect(spr, id, clazz.pos)
          val app           = TreeFactories.mkApply(slct, Nil, clazz.pos)
          val body          = TreeFactories.mkBlock(List(app), clazz.pos)
          val ret           = TreeFactories.mkTypeUse(clazz.name, clazz.pos)
          val const         = TreeFactories.mkMethodDef(mods, ret,
            constructorName, Nil, body, clazz.pos)
          val temp          = TreeCopiers.copyTemplate(clazz.body)(members =
            const::clazz.body.members)
          assign((temp, Some(sym))).asInstanceOf[TemplateApi]
      }
    }
    clazz.symbol = sym
    owner.foreach(owner => {
      clazz.owner = owner
      owner.declare(sym)
    })
    TreeCopiers.copyClassDef(clazz)(parents = parents, body = template)
  }


  protected def constructorName: Name       = StdNames.CONSTRUCTOR_NAME
  protected def voidSymbol: Option[Symbol]  = Some(VoidSymbol)

  protected def isConstructor(tree: Tree): Boolean =
    TreeUtils.isConstructor(tree)
}


@component(tree, owner)
trait BlockSymbolAssignerComponent extends
  primj.namers.BlockSymbolAssignerComponent {
  (block: BlockApi)          => {
    val res = super.apply((block, owner)).asInstanceOf[BlockApi]
    owner match {
      case Some(_: ClassSymbol)   =>
        res.isStaticInit = true
      case _                      =>
        ()
    }
    res
  }
}

@component(tree, owner)
trait TemplateSymbolAssignerComponent extends SymbolAssignerComponent {
  (tmpl: TemplateApi) => {
    val members  = tmpl.members.map(member =>
        assign((member, owner)))
    owner.foreach(tmpl.owner = _)
    TreeCopiers.copyTemplate(tmpl)(members = members)
  }
}

@component(tree, owner)
trait NewSymbolAssignerComponent extends SymbolAssignerComponent {
  (nu: NewApi) => {
    val app      = assign((nu.app, owner)).asInstanceOf[ApplyApi]
    owner.foreach(nu.owner = _)
    TreeCopiers.copyNew(nu)(app = app)
  }
}

@component(tree, owner)
trait SelectSymbolAssignerComponent extends SymbolAssignerComponent {
  (select: SelectApi) => {
    owner.foreach(select.owner = _)
    val qual           = assign((select.qual, owner))
    val slctdOwner     = qual.symbol
    val slctdEnclosing = owner
    slctdEnclosing.foreach(select.tree.enclosing = _)
    select.tree.isQualified = true
    val tree           =
      assign((select.tree, slctdOwner)).asInstanceOf[SimpleUseTree]
    TreeCopiers.copySelect(select)(tree = tree, qual = qual)
  }
}

@component(tree, owner)
trait ThisSymbolAssignerComponent extends SymbolAssignerComponent {
  (ths: ThisApi) => {
    owner.foreach(ths.owner = _)
    val encl = enclosingClass(owner)
    encl.foreach(ths.enclosingClassSymbol = _)
    ths
  }

  def enclosingClass(owner: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(owner)
}

@component(tree, owner)
trait SuperSymbolAssignerComponent extends SymbolAssignerComponent {
  (ths: SuperApi) => {
    owner.foreach(ths.owner = _)
    val enclosingClass = SymbolUtils.enclosingClass(owner)
    enclosingClass.foreach(ths.enclosingClassSymbol = _)
    ths
  }
}

@component(tree, owner)
trait MethodDefSymbolAssignerComponent
    extends primj.namers.MethodDefSymbolAssignerComponent {

  (mthd: MethodDefApi)          => {
    val symbol  = MethodSymbol(mthd.mods, mthd.name,
      Nil, None, None, owner)
    owner.foreach(sym => sym.declare(symbol))
    val opsym   = Some(symbol)
    val tpt     = if(mthd.name == constructorName) {
      mthd.declaredClassNameForConstructor = useName(mthd.ret)
      assign((TreeFactories.mkTypeUse(voidName, mthd.pos), owner)).asInstanceOf[UseTree]
    } else {
      assign((mthd.ret, owner)).asInstanceOf[UseTree]
    }
    val params  = mthd.params.map((x) =>
        assign((x, opsym)).asInstanceOf[ValDefApi])
    val body    = assign((mthd.body, opsym)).asInstanceOf[Expr]
    symbol.params = params.map(_.symbol).flatten
    symbol.ret    = tpt.symbol
    mthd.symbol = symbol
    symbol.owner.foreach(mthd.owner = _)

    TreeCopiers.copyMethodDef(mthd)(ret = tpt,
      params = params, body = body)
  }

  protected def useName(use: UseTree): Name = use match {
    case id: Ident      => id.name
    case tuse: TypeUse  => tuse.name
    case Select(_, i)   => i.name
  }
  protected def voidName: Name              = StdNames.VOID_TYPE_NAME
  protected def constructorName: Name       = StdNames.CONSTRUCTOR_NAME
}


@component(tree, owner)
trait ValDefSymbolAssignerComponent
  extends primj.namers.ValDefSymbolAssignerComponent {
  (valdef: ValDefApi) => {
    enclosingClass(owner) match {
      case Some(sym) if sym.mods.isInterface =>
        val mods = STATIC | FINAL | valdef.mods
        val nv   = TreeCopiers.copyValDef(valdef)(mods = mods)
        super.apply((nv, owner))
      case _                                 =>
        super.apply((valdef, owner))
    }
  }

  protected def enclosingClass(symbol: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(symbol)
}


// @component(tree, owner)
// trait IdentSymbolAssignerComponent extends SymbolAssignerComponent {
//   (id: Ident)          => {
//     owner.foreach(id.owner = _)
//     id
//   }
// }
//
// @component(tree, owner)
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
