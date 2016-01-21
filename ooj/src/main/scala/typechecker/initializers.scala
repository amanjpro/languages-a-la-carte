package ch.usi.inf.l3.sana.ooj.typechecker

import ch.usi.inf.l3.sana
import sana.ooj
import sana.primj
import sana.tiny


import ooj.ast._
import primj.ast.{BlockApi, ValDefApi}
import tiny.ast.{Tree, IdentApi}
import tiny.symbols.Symbol
import ooj.modifiers.Ops._
import ooj.ast.Implicits._
import ooj.symbols.SymbolUtils
import tiny.errors.ErrorReporting.{error,warning}
import ooj.errors.ErrorCodes._

import sana.dsl._
import sana.core._



trait InitializerCheckerComponent
  extends CheckerComponent[(Tree, List[Symbol])] {
  def check: ((Tree, List[Symbol])) => Unit
}



@component(tree, symbols)
trait ProgramInitializerCheckerComponent extends InitializerCheckerComponent {
  (prg: ProgramApi) => {
    prg.members.foreach(x => check((x, symbols)))
  }
}

@component(tree, symbols)
trait CompilationUnitInitializerCheckerComponent
  extends InitializerCheckerComponent {
  (unit: CompilationUnitApi) => {
    check((unit.module, symbols))
  }
}

@component(tree, symbols)
trait PackageDefInitializerCheckerComponent
  extends InitializerCheckerComponent {
  (pkg: PackageDefApi) => {
    pkg.members.foreach(x => check((x, symbols)))
  }
}

@component(tree, symbols)
trait ClassDefInitializerCheckerComponent extends InitializerCheckerComponent {
  (clazz: ClassDefApi) => {
    check((clazz.body, symbols))
  }
}

@component(tree, symbols)
trait TemplateInitializerCheckerComponent extends InitializerCheckerComponent {
  (template: TemplateApi) => {
    template.members.foldLeft(Nil: List[Symbol]) { (z, member) => member match {
      case v: ValDefApi if v.mods.isStatic && v.mods.isField =>
        v.symbol.map(_::z).getOrElse(z)
      case block: BlockApi if block.isStaticInit             =>
        check((block, z))
        z
      case _                                                 =>
        z
    }}
  }
}


@component(tree, symbols)
trait BlockInitializerCheckerComponent extends InitializerCheckerComponent {
  (block: BlockApi) => {
    val clazz = enclosingClass(block.owner)
    if(block.isStaticInit) {
      block.bottomUp(()) ( (z, t) => t match {
          case id: IdentApi                    =>
            id.symbol.foreach { sym =>
              if(sym.mods.isField && sym.mods.isStatic &&
                  clazz == sym.owner &&
                  !symbols.contains(sym))
                error(FIELD_FORWARD_REFERENCE_IN_STATIC_INIT,
                  "", "", t.pos)
            }
          case _                               =>
            ()
        }
      )
    }
  }

  protected def enclosingClass(sym: Option[Symbol]): Option[Symbol] =
    SymbolUtils.enclosingClass(sym)
}
