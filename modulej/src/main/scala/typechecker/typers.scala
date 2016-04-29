package ch.usi.inf.l3.sana.modulej.typechecker

import ch.usi.inf.l3.sana
import sana.modulej
import sana.ooj
import sana.arrooj
import sana.calcj
import sana.tiny
import sana.ppj
import sana.robustj

import tiny.core.{CompilerInterface, TransformationComponent}
import tiny.dsl._


import modulej.ast._
import modulej.ast.Implicits._
import ooj.ast.{PackageDefApi, SelectApi,
                CompilationUnitApi => OCompilationUnitApi}
import tiny.ast.{IdentApi, TypeUseApi, UseTree, NoTree, Tree}
import robustj.ast.{MethodDefApi}
import tiny.errors.ErrorReporting.{error,warning}
import modulej.errors.ErrorCodes._
import modulej.modifiers.Ops._
import calcj.typechecker.TyperComponent


// @component
// trait ImportTyperComponent extends TyperComponent {
//   (imprt: ImportApi) => {
//     val qual = typed(imprt.qual).asInstanceOf[UseTree]
//     qual.symbol match {
//       case None       if !imprt.isOnDemand                     =>
//         error(IMPORTED_PACKAGE_IS_MISSING,
//           "", "", qual.pos)
//       case None                                                =>
//         error(IMPORTED_CLASS_IS_MISSING,
//           "", "", qual.pos)
//       case _                                                   =>
//         ()
//     }
//     TreeCopiers.copyImport(imprt)(qual = qual)
//   }
// }

@component
trait CompilationUnitTyperComponent extends TyperComponent {
  (unit: OCompilationUnitApi) => {
    unit match {
      case unit: CompilationUnitApi       =>
        val pkg     = typed(unit.module).asInstanceOf[PackageDefApi]
        // val imports = unit.imports.map(typed(_).asInstanceOf[ImportApi])
        TreeCopiers.copyCompilationUnit(unit)(module = pkg)
      case unit: OCompilationUnitApi      =>
        val res = TreeUpgraders.upgradeCompilationUnit(unit)
        typed(res)
    }
  }
}


@component
trait IdentTyperComponent extends ooj.typechecker.IdentTyperComponent {
  (id: IdentApi)          => {
    attachQualifiedNameAttribute(id)
    super.apply(id)
  }
  override protected def nameIdent(id: IdentApi): UseTree =
    identNamer.nameIdent(id)

  override protected def typeAndNameIdent(id: IdentApi): UseTree =
    identNamer.nameIdent(id, true)


  private[this] val identNamer = {
    val comp = this
    new modulej.namers.IdentNamer with ooj.typechecker.IdentNamer {
      protected val compiler: CompilerInterface = comp.compiler
      def family(use: UseTree): UseTree = comp.typed(use).asInstanceOf[UseTree]
    }
  }
  protected def attachQualifiedNameAttribute(use: UseTree): Unit =
    TreeUtils.attachQualifiedNameAttribute(use)
}

@component
trait TypeUseTyperComponent
  extends ooj.typechecker.TypeUseTyperComponent {
  (tuse: TypeUseApi) => {
    attachQualifiedNameAttribute(tuse)
    val tuseCopy = if(!tuse.hasBeenNamed) {
      typeUseNamer.nameTypeUse(tuse)
    } else tuse
    tuseCopy match {
      case tuse: TypeUseApi              =>
        super.apply(tuseCopy)
      case _                             =>
        typed(tuseCopy)
    }
  }

  override protected def nameTypeUse(tuse: TypeUseApi): UseTree =
    typeUseNamer.nameTypeUse(tuse)

  private[this] val typeUseNamer = {
    val comp = this
    new modulej.namers.TypeUseNamer {
      protected val compiler: CompilerInterface = comp.compiler
      def family(use: UseTree): UseTree =
        comp.typed(use).asInstanceOf[UseTree]
    }
  }
  protected def attachQualifiedNameAttribute(use: UseTree): Unit =
    TreeUtils.attachQualifiedNameAttribute(use)
}


@component
trait SelectTyperComponent extends arrooj.typechecker.SelectTyperComponent {
  (slct: SelectApi) => {
    attachQualifiedNameAttribute(slct)
    super.apply(slct)
  }

  protected def attachQualifiedNameAttribute(use: UseTree): Unit =
    TreeUtils.attachQualifiedNameAttribute(use)
}



@component
trait MethodDefTyperComponent extends
    ppj.typechecker.MethodDefTyperComponent {

  (mthd: MethodDefApi) => {
    val mthd2 = super.apply(mthd).asInstanceOf[MethodDefApi]

    if(mthd2.mods.isNative && !isConstructor(mthd.symbol) &&
          mthd2.body != NoTree) {
      error(NATIVE_METHOD_CANNOT_HAVE_BODY,
          "", "", mthd.pos)
    }

    if(mthd2.mods.isNative && isConstructor(mthd.symbol)) {
      error(CONSTRUCTOR_CANNOT_BE_NATIVE,
          "", "", mthd.pos)
    }


    mthd2
  }

  override def allPathsReturn(expr: Tree): Boolean = {
    enclosingMethod(expr.symbol) match {
      case Some(mthd)                         =>
        mthd.mods.isNative || super.allPathsReturn(expr)
      case None                               =>
        super.allPathsReturn(expr)
    }
  }
}
