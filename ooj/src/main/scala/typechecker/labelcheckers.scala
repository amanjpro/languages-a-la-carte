package ch.usi.inf.l3.sana.ooj.typechecker

import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.core.CheckerComponent
import tiny.dsl._
import primj.ast.Implicits._
import primj.ast.{MethodDefApi => PMethodDefApi}
import tiny.names.Name
import calcj.ast.operators.{Inc, Dec}
import tiny.errors.ErrorReporting.{error,warning}
import primj.symbols._
import primj.modifiers.Ops._
import primj.typechecker.ShapeCheckerComponent
import brokenj.typechecker.LabelNameCheckerComponent
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

@component(tree, labelNames)
trait ProgramLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (prg: ProgramApi)  => {
    prg.members.foreach { member => check((member, labelNames)) }
  }
}

@component(tree, labelNames)
trait CompilationUnitLabelNameCheckerComponent extends
    LabelNameCheckerComponent {
  (cunit: CompilationUnitApi)  => {
    check((cunit.module, labelNames))
  }
}

@component(tree, labelNames)
trait PackageDefLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (pkg: PackageDefApi)  => {
    pkg.members.foreach { member => check((member, labelNames)) }
  }
}

@component(tree, labelNames)
trait ClassDefLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (clazz: ClassDefApi)  => {
    clazz.parents.foreach { parent => check((parent, labelNames)) }
    check((clazz.body, labelNames))
  }
}

@component(tree, labelNames)
trait TemplateLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (template: TemplateApi)  => {
    template.members.foreach { member => check((member, labelNames)) }
  }
}

@component(tree, labelNames)
trait NewLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (nw: NewApi)  => {
    check((nw.app, labelNames))
  }
}

@component(tree, labelNames)
trait SelectLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (select: SelectApi)  => {
    check((select.qual, labelNames))
    check((select.tree, labelNames))
  }
}

@component(tree, labelNames)
trait ThisLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (ths: ThisApi)  => ths
}

@component(tree, labelNames)
trait SuperLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (spr: SuperApi)  => spr
}

@component(tree, labelNames)
trait MethodDefLabelNameCheckerComponent extends
    brokenj.typechecker.MethodDefLabelNameCheckerComponent {
  (mthd: PMethodDefApi)  => super.apply((mthd, labelNames))
}
