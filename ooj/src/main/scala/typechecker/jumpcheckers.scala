package ch.usi.inf.l3.sana.ooj.typechecker

import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.CheckerComponent
import sana.dsl._
import primj.ast.Implicits._
import tiny.names.Name
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.symbols._
import primj.modifiers.Ops._
import primj.typechecker.ShapeCheckerComponent
import brokenj.typechecker.JumpCheckerComponent
import brokenj.errors.ErrorCodes._
import brokenj.ast.TreeUtils

import ooj.ast._



/**
CompilationUnit: DONE
Program: DONE
PackageDef: DONE
ClassDef: DONE
Template: DONE
New: DONE
Select: DONE
This: DONE
Super: DONE
MethodDef: DONE
*/

@component(tree, encls)
trait ProgramJumpCheckerComponent extends JumpCheckerComponent {
  (prg: ProgramApi)  => {
    prg.members.foreach { member => check((member, Nil)) }
  }
}

@component(tree, encls)
trait CompilationUnitJumpCheckerComponent extends
    JumpCheckerComponent {
  (cunit: CompilationUnitApi)  => {
    check((cunit.module, encls))
  }
}

@component(tree, encls)
trait PackageDefJumpCheckerComponent extends JumpCheckerComponent {
  (pkg: PackageDefApi)  => {
    pkg.members.foreach { member => check((member, encls)) }
  }
}

@component(tree, encls)
trait ClassDefJumpCheckerComponent extends JumpCheckerComponent {
  (clazz: ClassDefApi)  => {
    clazz.parents.foreach { parent => check((parent, encls)) }
    check((clazz.body, encls))
  }
}

@component(tree, encls)
trait TemplateJumpCheckerComponent extends JumpCheckerComponent {
  (template: TemplateApi)  => {
    template.members.foreach { member => check((member, encls)) }
  }
}

@component(tree, encls)
trait NewJumpCheckerComponent extends JumpCheckerComponent {
  (nw: NewApi)  => {
    check((nw.app, encls))
  }
}

@component(tree, encls)
trait SelectJumpCheckerComponent extends JumpCheckerComponent {
  (select: SelectApi)  => {
    check((select.qual, encls))
    check((select.tree, encls))
  }
}

@component(tree, encls)
trait ThisJumpCheckerComponent extends JumpCheckerComponent {
  (ths: ThisApi)  => ths
}

@component(tree, encls)
trait SuperJumpCheckerComponent extends JumpCheckerComponent {
  (spr: SuperApi)  => spr
}

@component(tree, encls)
trait MethodDefJumpCheckerComponent extends
    brokenj.typechecker.MethodDefJumpCheckerComponent {
  (mthd: MethodDefApi)  => super.apply((mthd, encls))
}