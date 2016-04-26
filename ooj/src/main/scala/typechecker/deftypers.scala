package ch.usi.inf.l3.sana.ooj.typechecker


import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.TransformationComponent
import tiny.dsl._
import tiny.ast.{TreeCopiers => _, _}
import tiny.types._
import tiny.types.TypeUtils._
import tiny.symbols.{TypeSymbol, TermSymbol}
import tiny.source.Position
import tiny.errors.ErrorReporting.{error,warning}
import calcj.typechecker.{TyperComponent, TypePromotions}
import calcj.ast.{TreeCopiers => _, _}
import calcj.types._
// import primj.symbols._
import primj.errors.ErrorCodes._
import primj.types._
import primj.symbols.{MethodSymbol, VariableSymbol}
import primj.ast.{TreeCopiers => _, MethodDefApi => PMethodDefApi,
                  ProgramApi => _, TreeUtils => _, _}
import primj.modifiers.Ops._
import ooj.ast.Implicits._
import brokenj.ast.{TreeCopiers => _, TreeUtils => _, _}
import ooj.ast._
import ooj.symbols._
import ooj.types._

/*
Needed:
=======


CompilationUnit: DONE
PackageDef: DONE
Template: DONE

ClassDef: DONE
MethodDef: DONE
ValDef: DONE
TypeUse: DONE
Select: DONE
Ident: DONE




Unneeded:
=========
Ident:
New:
Case:
Swtich:
Label:
Break:
Continue:
Assign:
If:
While:
Block:
For:
Ternary:
Apply:
Return:
NoTree:
Cast:
Binary:
Literal: DONE
Unary:
This:
Super:
*/


trait DefTyperComponent extends TransformationComponent[Tree, Tree] {
  def typed: Tree => Tree
}

@component
trait ProgramDefTyperComponent extends DefTyperComponent {
  (prg: ProgramApi) => {
    val members =
      prg.members.map(x => typed(x))
    TreeCopiers.copyProgram(prg)(members = members)
  }
}

@component
trait CompilationUnitDefTyperComponent extends DefTyperComponent {
  (unit: CompilationUnitApi) => {
    val pkg = typed(unit.module).asInstanceOf[PackageDefApi]
    TreeCopiers.copyCompilationUnit(unit)(module = pkg)
  }
}

@component
trait PackageDefDefTyperComponent extends DefTyperComponent {
  (pkg: PackageDefApi) => {
    val members =
      pkg.members.map(x => typed(x).asInstanceOf[DefTree])
    TreeCopiers.copyPackageDef(pkg)(members = members)
  }
}


@component
trait ClassDefDefTyperComponent extends DefTyperComponent {
  (clazz: ClassDefApi) => {
    val body    = typed(clazz.body).asInstanceOf[TemplateApi]
    TreeCopiers.copyClassDef(clazz)(body = body)
  }


  protected def packageName(symbol: ClassSymbol): String =
    SymbolUtils.packageName(symbol)
}



@component
trait MethodDefDefTyperComponent extends DefTyperComponent {
  (mthd: PMethodDefApi)          => {
    mthd match {
      case mthd: MethodDefApi          =>
        val tpt     = typed(mthd.ret).asInstanceOf[UseTree]
        val params  = mthd.params.map(typed(_).asInstanceOf[ValDefApi])
        val tparams = params.map(_.tpe.getOrElse(ErrorType))
        val rtpe    = tpt.tpe.getOrElse(ErrorType)
        val tpe = MethodType(rtpe, tparams)
        mthd.tpe = tpe
        mthd.symbol.foreach( sym => {
          sym match {
            case m: MethodSymbol    =>
              m.ret = tpt.symbol
            case _                  =>
              ()
          }
          sym.tpe = Some(tpe)
        })
        TreeCopiers.copyMethodDef(mthd)(ret = tpt, params = params)
      case mthd: PMethodDefApi          =>
        val res = TreeUpgraders.upgradeMethodDef(mthd)
        typed(res)
    }
  }
}



@component
trait TemplateDefTyperComponent extends DefTyperComponent {
  (template: TemplateApi) => {
    val members = template.members.map(typed(_))
    TreeCopiers.copyTemplate(template)(members = members)
  }
}


@component
trait ValDefDefTyperComponent extends DefTyperComponent {
  (valdef: ValDefApi)          => {
    val tpt     = typed(valdef.tpt).asInstanceOf[UseTree]
    val rtpe    = tpt.tpe.getOrElse(ErrorType)
    valdef.tpe  = rtpe
    valdef.symbol.foreach {sym =>
      sym match {
        case vs: VariableSymbol    =>
          vs.typeSymbol = tpt.symbol
        case _                     =>
          ()
      }
    }
    TreeCopiers.copyValDef(valdef)(tpt = tpt)
  }
}


// Here we need selct and typeuse

@component
trait SelectDefTyperComponent extends DefTyperComponent {
  (select: SelectApi) => {
    val qual = typed(select.qual)
    qual.symbol.foreach(select.tree.owner = _)
    val tree = typed(select.tree).asInstanceOf[SimpleUseTree]
    tree.tpe.foreach(select.tpe = _)
    tree.symbol.foreach(select.symbol = _)
    if(isTypeUse(qual)) {
      tree.shouldBeStatic = true
    }
    TreeCopiers.copySelect(select)(qual, tree)
  }


  protected def isTypeUse(tree: Tree): Boolean = tree match {
    case t: UseTree => TreeUtils.isTypeUse(t)
    case _          => false
  }
}

@component
trait TypeUseDefTyperComponent extends DefTyperComponent {
  (tuse: TypeUseApi) => {
    tuse.symbol.foreach { sym =>
      // case None                      =>
        // tuse.owner.foreach(sym => {
        //   sym.getSymbol(tuse.name, _.isInstanceOf[TypeSymbol]) match {
        //     case Some(sym) =>
        //       tuse.symbol = sym
        //       sym.tpe.foreach(tuse.tpe = _)
        //     case _         =>
        //       ()
        //   }
        // })
      // case Some(sym)                 =>
      sym.tpe.foreach(tuse.tpe = _)
    }
    tuse
  }
}

@component
trait IdentDefTyperComponent extends DefTyperComponent {
  (id: IdentApi) => {
    id.symbol.foreach { sym =>
      // case None                      =>
      //   id.owner.foreach(sym => {
      //     sym.getSymbol(id.name, _.isInstanceOf[TermSymbol]) match {
      //       case Some(sym) =>
      //         id.symbol = sym
      //         sym.tpe.foreach(id.tpe = _)
      //       case _         => ()
      //     }
      //   })
      // case Some(sym)                 =>
      sym.tpe.foreach(id.tpe = _)
    }
    id
  }
}


@component
trait BlockDefTyperComponent extends DefTyperComponent {
  (block: BlockApi) => block
}
