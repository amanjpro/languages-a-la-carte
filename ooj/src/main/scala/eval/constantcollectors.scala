package ch.usi.inf.l3.sana.ooj.eval

import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.TransformationComponent
import sana.dsl._


import tiny.symbols.Symbol
import ooj.ast._
import ooj.ast.Implicits._
import ooj.ast.TreeExtractors._
import ooj.modifiers.Ops._
import ooj.symbols.SymbolUtils
import brokenj.ast.{TreeCopiers => _, TreeUtils => _, _}
import primj.ast.{TreeCopiers => _, MethodDefApi => _,
                  ProgramApi => _, TreeUtils => _, _}
import calcj.ast.{TreeCopiers => _, _}
import tiny.ast.{TreeCopiers => _, _}



trait ConstantCollectingComponent
  extends TransformationComponent[(Tree, Env), (Tree, Env)] {
  def collect: ((Tree, Env)) => (Tree, Env)
}


@component(tree, env)
trait ProgramConstantCollectingComponent extends ConstantCollectingComponent {
  (prg: ProgramApi)  => {
    val (members, newEnv) = prg.members.foldLeft((Nil: List[Tree], env)){
      (z, member) =>
        val e       = z._2
        val members = z._1
        val (res, env) = collect((member, e))
        (members++List(res), env)
    }
    (TreeCopiers.copyProgram(prg)(members = members), newEnv)
  }
}

@component(tree, env)
trait CompilationUnitConstantCollectingComponent extends
    ConstantCollectingComponent {
  (cunit: CompilationUnitApi)  => {
    val (module, newEnv) = collect((cunit.module, env))
    (TreeCopiers.copyCompilationUnit(cunit)(module =
        module.asInstanceOf[PackageDefApi]), newEnv)
  }
}

@component(tree, env)
trait PackageDefConstantCollectingComponent
  extends ConstantCollectingComponent {
  (pkg: PackageDefApi)  => {
    val (members, newEnv) = pkg.members.foldLeft((Nil: List[Tree], env)) {
      (z, member) =>
        val e       = z._2
        val members = z._1
        val (res, env) = collect((member, e))
        (members++List(res), env)

    }
    (TreeCopiers.copyPackageDef(pkg)(members = members), newEnv)
  }
}

@component(tree, env)
trait ClassDefConstantCollectingComponent
  extends ConstantCollectingComponent {
  (clazz: ClassDefApi)  => {
    val (body, newEnv) = collect((clazz.body, env))
    (TreeCopiers.copyClassDef(clazz)(body = body.asInstanceOf[TemplateApi]),
        newEnv)
  }
}

@component(tree, env)
trait TemplateConstantCollectingComponent
  extends ConstantCollectingComponent {
  (template: TemplateApi)  => {
    val (members, newEnv) = template.members.foldLeft((Nil: List[Tree], env)){
    (z, member) =>
      val e       = z._2
      val members = z._1
      val (res, env) = collect((member, e))
      (members++List(res), env)
    }
    (TreeCopiers.copyTemplate(template)(members = members), newEnv)
  }
}


@component(tree, env)
trait ValDefConstantCollectingComponent
  extends ConstantCollectingComponent {
  (valdef: ValDefApi) => {
    if(valdef.mods.isFinal && valdef.mods.isField &&
        valdef.rhs != NoTree ) {
      val newEnv2 = valdef.symbol.map { sym =>
        env.bind(sym, ExprValue(valdef.rhs))
      }.getOrElse(env)
      (valdef, newEnv2)
    } else (valdef, env)
  }

}



@component(tree, env)
trait MethodDefConstantCollectingComponent
  extends ConstantCollectingComponent {
  (mthd: MethodDefApi) => (mthd, env)
}

@component(tree, env)
trait BlockConstantCollectingComponent
  extends ConstantCollectingComponent {
  (block: BlockApi) => (block, env)
}

// @component(tree, env)
// trait SelectConstantCollectingComponent
//   extends ConstantCollectingComponent {
//   (select: SelectApi) => {
//     if(isTypeSymbol(select.qual.symbol) &&
//         isStatic(select.tree.symbol)) {
//       select.symbol.map { sym =>
//         env.getValue(sym) match {
//           case TypeValue(tenv)          =>
//             val (v, _) = collect((select.tree, tenv))
//             if(isConstantExpression(v)) {
//               (v, env)
//             } else {
//               (select, env)
//             }
//           case _                        =>
//             (select, env)
//         }
//       }.getOrElse((select, env))
//     } else {
//       (select, env)
//     }
//   }
//
//
//   def isTypeSymbol(sym: Option[Symbol]): Boolean =
//     SymbolUtils.isTypeSymbol(sym)
//
//   def isStatic(sym: Option[Symbol]): Boolean =
//     sym.map(_.mods.isStatic).getOrElse(false)
//
//   protected def isConstantExpression(tree: Tree): Boolean =
//     TreeUtils.isConstantExpression(tree)
// }
//
//
// @component(tree, env)
// trait IdentConstantCollectingComponent
//   extends ConstantCollectingComponent {
//   (ident: IdentApi)    => {
//     ident.symbol.map { sym =>
//       env.getValue(sym) match {
//         case ExprValue(v) =>
//           (v, env)
//         case _            =>
//           (ident, env)
//       }
//     }.getOrElse((ident, env))
//   }
//
//   protected def isConstantExpression(tree: Tree): Boolean =
//     TreeUtils.isConstantExpression(tree)
// }
