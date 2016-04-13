package ch.usi.inf.l3.sana.arrayj.namers

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj
import sana.arrayj

import tiny.core.TransformationComponent
import tiny.dsl._
import tiny.ast.{TreeCopiers => _, _}
import primj.ast.Implicits._
import tiny.errors.ErrorReporting.{error,warning}
import tiny.symbols._
import calcj.ast.{TreeCopiers => _, _}
import primj.ast.{TreeCopiers => _, _}
import primj.symbols._
import primj.modifiers.Ops._
import primj.errors.ErrorCodes._
import primj.namers.NamerComponent
import arrayj.ast._

/*
ArrayTypeUse: DONE
*/



@component
trait ArrayTypeUseNamerComponent extends NamerComponent {
  (tuse: ArrayTypeUseApi) => {
    val tpt = name(tuse.tpt).asInstanceOf[UseTree]

    TreeCopiers.copyArrayTypeUse(tuse)(tpt = tpt)
  }
}

