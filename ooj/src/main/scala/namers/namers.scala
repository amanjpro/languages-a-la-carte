package ch.usi.inf.l3.sana.ooj.namers


import ch.usi.inf.l3.sana
import sana.ooj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast.{TreeCopiers => _, TreeFactories => _, _}
import tiny.names.Name
import tiny.symbols._
import tiny.errors.ErrorReporting.{error,warning}
import calcj.ast.{TreeCopiers => _, TreeFactories => _, _}
import calcj.ast.operators.{Inc, Dec}
import primj.namers.NamerComponent
import primj.symbols.{SymbolUtils => _, _}
import primj.errors.ErrorCodes._
import ooj.ast._
import ooj.names.StdNames
import ooj.modifiers._
import ooj.modifiers.Ops._
import ooj.symbols.{SymbolUtils, ClassSymbol}
import ooj.ast.Implicits._


/*
CompilationUnit: DONE
PackageDef: DONE
ClassDef: DONE
Template: DONE
New: DONE
Select: DONE
This: DONE
Super: DONE -- I think
MethodDef: DONE
Apply:
*/


@component
trait CompilationUnitNamerComponent extends NamerComponent {
  (unit: CompilationUnitApi) => {
    val pkg = name(unit.module).asInstanceOf[PackageDefApi]
    TreeCopiers.copyCompilationUnit(unit)(module = pkg)
  }
}

@component
trait PackageDefNamerComponent extends NamerComponent {
  (pkg: PackageDefApi) => {
    val newMembers =
      pkg.members.map(x => name(x).asInstanceOf[DefTree])
    TreeCopiers.copyPackageDef(pkg)(members = newMembers)
  }
}


@component
trait ClassDefNamerComponent extends NamerComponent {
  (clazz: ClassDefApi) => {
    // If there is no explicit Object parent, add it
    // implicitly
    val parents = {
      val temp = clazz.parents.map((parent) =>
          name(parent).asInstanceOf[UseTree])
      val sym  = objectClassSymbol
      temp.exists(_.symbol == Some(sym)) match {
        case true       => temp
        case _          =>
          val tuse = TreeFactories.mkTypeUse(sym.name,
            clazz.pos,
            Some(sym),
            None,
            sym.owner)
          tuse::temp
      }
    }
    val body    = {
      val temp = name(clazz.body).asInstanceOf[TemplateApi]
      // TODO: Do we need to have a refactoring for creating constructors?
      // I would say yes!!!
      // INFO: No constructors? Add it!
      temp.members.exists(isConstructor(_)) match {
        case true                => temp
        case false               =>
          val mods          = {
            if(clazz.mods.isPublicAcc)         CONSTRUCTOR | PUBLIC_ACC
            else if(clazz.mods.isProtectedAcc) CONSTRUCTOR | PROTECTED_ACC
            else if(clazz.mods.isPrivateAcc)   CONSTRUCTOR | PRIVATE_ACC
            else                               CONSTRUCTOR | PACKAGE_ACC
          }
          val mthdSymbol    = Some(MethodSymbol(mods, constructorName,
            Nil, None, clazz.symbol))
          val spr           = TreeFactories.mkSuper(clazz.pos,
            clazz.symbol, mthdSymbol)
          val app           = TreeFactories.mkApply(spr, Nil,
            clazz.pos, mthdSymbol)
          val body          = TreeFactories.mkBlock(List(app), clazz.pos,
            mthdSymbol)
          val name          = constructorName
          val ret           = TreeFactories.mkTypeUse(voidName, clazz.pos,
            voidSymbol, clazz.symbol, clazz.symbol)
          val const        = TreeFactories.mkMethodDef(mods,
            ret, name, Nil, body, clazz.pos, mthdSymbol)
          TreeCopiers.copyTemplate(temp)(members = const::temp.members)
      }
    }
    TreeCopiers.copyClassDef(clazz)(body = body, parents = parents)
  }

  protected def objectClassSymbol: ClassSymbol =
    SymbolUtils.objectClassSymbol

  protected def constructorName: Name       = StdNames.CONSTRUCTOR_NAME
  protected def voidName: Name              = StdNames.VOID_TYPE_NAME
  protected def voidSymbol: Option[Symbol]  = Some(VoidSymbol)
  protected def isConstructor(tree: Tree): Boolean =
    TreeUtils.isConstructor(tree)
}

@component
trait TemplateNamerComponent extends NamerComponent {
  (template: TemplateApi) => {
    val members = template.members.map(name(_))
    TreeCopiers.copyTemplate(template)(members = members)
  }
}


@component
trait MethodDefNamerComponent extends primj.namers.MethodDefNamerComponent {
  (mthd: MethodDefApi) => {
    val res  = super.apply(mthd).asInstanceOf[primj.ast.MethodDefApi]
    // INFO: a bit of hack, but works
    val res2 = TreeFactories.mkMethodDef(mthd.mods, res.ret, res.name,
      res.params, res.body)
    res2.attributes = res.attributes
    res2
  }
}


@component
trait ThisNamerComponent extends NamerComponent {
  (ths: ThisApi) => ths
}

@component
trait SuperNamerComponent extends NamerComponent {
  (spr: SuperApi) => spr
}

@component
trait SelectNamerComponent extends NamerComponent {
  (select: SelectApi) => {
    val qual    = name(select.qual)
    val tree    = name(select.tree).asInstanceOf[SimpleUseTree]
    TreeCopiers.copySelect(select)(qual = qual, tree = tree)
  }
}

@component
trait NewNamerComponent extends NamerComponent {
  (nw: New) => {
    val app     = name(nw.app).asInstanceOf[ApplyApi]
    TreeCopiers.copyApply(nw)(app = app)
  }
}

trait IdentNamerComponent extends primj.namers.IdentNamerComponent {
  (id: IdentApi)       => {
    // At the beginning: we treat all (Ident)s as ambiguous names.
    // Can we see any (VariableSymbol)s with this name from the current scope?
    val temp = id.owner.flatMap(_.getSymbol(id.name,
      _.isInstanceOf[VariableSymbol]))
    temp match {
      case None        =>
        // Can we see any (TypeSymbol)s with this name from the current scope?
        val temp = id.owner.flatMap(_.getSymbol(id.name,
          _.isInstanceOf[TypeSymbol]))
          temp match {
            case None        =>
              // TODO: Look for imports later when we introduce
              // them: Section 6.5.2
              id
            case Some(sym)   =>
              id.symbol = sym
              sym.tpe.foreach(id.tpe = _)
              val tuse = TreeFactories.mkTypeUse(id.name, id.pos, id.symbol, id.owner)
              tuse.attributes = id.attributes
              tuse
          }
      case Some(sym)   =>
        id.symbol = sym
        sym.tpe.foreach(id.tpe = _)
        id
    }
  }
}



