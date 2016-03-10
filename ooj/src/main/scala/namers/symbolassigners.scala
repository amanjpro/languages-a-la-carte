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
import tiny.source.Position
import tiny.names.Name
import calcj.ast.{TreeCopiers => _, TreeFactories => _, _}
import primj.ast.{TreeCopiers => _, MethodDefApi => _, ProgramApi => _,
                  TreeFactories => _, TreeUtils => _, _}
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
    val owner   = cunit.owner
    val sym     = CompilationUnitSymbol(None, cunit.sourceName,
                                  cunit.sourcePath, owner)
    owner.foreach(owner => {
      owner.declare(sym)
    })
    cunit.module.owner = sym
    val pkg            = assign(cunit.module).asInstanceOf[PackageDefApi]
    sym.module         = pkg.symbol
    TreeCopiers.copyCompilationUnit(cunit)(module = pkg)
  }
}

@component
trait PackageDefSymbolAssignerComponent extends SymbolAssignerComponent {
  (pkg: PackageDefApi) => {
    val owner = pkg.owner
    val (newOwner, sym) = owner match {
      case Some(cunit: CompilationUnitSymbol) =>
        val sym     = createSymbol(pkg.containingPackages,
          pkg.name, Some(ProgramSymbol))
        cunit.owner = Some(sym)
        (cunit, sym)
      case _                                  =>
        val sym     = createSymbol(pkg.containingPackages,
          pkg.name, owner)
        sym.owner = owner
        (sym, sym)
    }
    pkg.symbol = sym
    val members = pkg.members.map { member =>
      member.owner = newOwner
      assign(member)
    }
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }


  protected def createSymbol(names: List[Name], name: Name,
    owner: Option[Symbol]): Symbol = {
    names match {
      case (n::ns) if name != StdNames.DEFAULT_PACKAGE_NAME      =>
        val s = createSymbol(Nil, n, owner)
        createSymbol(ns, name, Some(s))
      case _                                                     =>
        owner.flatMap(_.getDirectlyDefinedSymbol(name,
            _.isInstanceOf[PackageSymbol])) match {
          case None               =>
            val sym = PackageSymbol(name, owner)
            owner.foreach(_.declare(sym))
            sym
          case Some(sym)          =>
            sym
        }

    }
  }
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
    val template = {
      val shouldCreateConstructor =
        !(clazz.body.members.exists(isConstructor(_))) &&
          ! clazz.mods.isInterface
      shouldCreateConstructor match {
        case false               =>
          clazz.body.owner = sym
          assign(clazz.body).asInstanceOf[TemplateApi]
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
          id.isConstructorIdent = true
          val slct          = TreeFactories.mkSelect(spr, id, clazz.pos)
          val app           = TreeFactories.mkApply(slct, Nil, clazz.pos)
          val body          = TreeFactories.mkBlock(List(app), clazz.pos)
          val ret           = TreeFactories.mkTypeUse(clazz.name, clazz.pos)
          val const         = TreeFactories.mkMethodDef(mods, ret,
            constructorName, Nil, body, clazz.pos)
          val temp          = TreeCopiers.copyTemplate(clazz.body)(members =
            const::clazz.body.members)
          temp.owner        = sym
          assign(temp).asInstanceOf[TemplateApi]
      }
    }
    classDoubleDefCheck(owner, clazz.name, clazz.pos)
    owner.foreach(_.declare(sym))
    TreeCopiers.copyClassDef(clazz)(parents = parents, body = template)
  }


  protected def createClassSymbol(clazz: ClassDefApi,
    owner: Option[Symbol]): ClassSymbol = owner match {
      case Some(cunit: CompilationUnitSymbol)
            if Some(clazz.name.asString) != clazz.sourceName              =>
        ClassSymbol(clazz.mods, clazz.name, Nil, owner, None)
      case Some(cunit: CompilationUnitSymbol)                             =>
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
          "", "", pos)
    case _                                                      =>
      ()
  }

  protected def constructorName: Name       = StdNames.CONSTRUCTOR_NAME
  protected def voidSymbol: Option[Symbol]  = Some(VoidSymbol)

  protected def isConstructor(tree: Tree): Boolean =
    TreeUtils.isConstructor(tree)
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
  (mthd: MethodDefApi)          => {
    val owner   = mthd.owner
    val symbol  = createMethodSymbol(mthd, owner)
    owner.foreach(sym => sym.declare(symbol))
    val tpt     = if(mthd.mods.isConstructor) {
      mthd.declaredClassNameForConstructor = useName(mthd.ret)
      val tuse = TreeFactories.mkTypeUse(voidName, mthd.pos)
      owner.foreach(tuse.owner = _)
      assign(tuse).asInstanceOf[UseTree]
    } else {
      owner.foreach(mthd.ret.owner = _)
      assign(mthd.ret).asInstanceOf[UseTree]
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
                    Ident(`constructorName`)), _), _*))                     =>
          mthd.body.owner = symbol
          assign(mthd.body).asInstanceOf[Expr]
        case body: BlockApi                                                 =>
          val newBody = TreeCopiers.copyBlock(body)(
            stmts = constructorCall(body.pos)::body.stmts)
          newBody.owner = symbol
          assign(newBody).asInstanceOf[Expr]
        case expr                                                           =>
          val newBody = TreeFactories.mkBlock(
            List(constructorCall(expr.pos), expr), expr.pos)
          newBody.owner = symbol
          assign(newBody).asInstanceOf[Expr]
      }
    } else {
      mthd.body.owner = symbol
      assign(mthd.body).asInstanceOf[Expr]
    }
    symbol.params = params.flatMap(_.symbol)
    symbol.ret    = tpt.symbol
    mthd.symbol = symbol
    TreeCopiers.copyMethodDef(mthd)(ret = tpt,
      params = params, body = body)
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

  protected def useName(use: UseTree): Name = use match {
    case id: IdentApi      => id.name
    case tuse: TypeUseApi  => tuse.name
    case Select(_, i)      => i.name
  }

  protected def voidName: Name              = StdNames.VOID_TYPE_NAME
  protected val constructorName: Name       = StdNames.CONSTRUCTOR_NAME
}


@component
trait ValDefSymbolAssignerComponent
  extends primj.namers.ValDefSymbolAssignerComponent {
  (valdef: ValDefApi) => {
    val owner = valdef.owner
    owner match {
      case Some(sym) if sym.mods.isInterface =>
        val mods = STATIC | FINAL | FIELD | valdef.mods
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
