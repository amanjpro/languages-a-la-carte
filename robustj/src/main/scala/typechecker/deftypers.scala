package ch.usi.inf.l3.sana.robustj.typechecker

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj
import sana.arrayj
import sana.arrooj
import sana.ooj
import sana.robustj

import tiny.core.TransformationComponent
import tiny.dsl._
import robustj.ast.{TreeFactories, MethodDefApi, TreeCopiers, TreeUpgraders}
import primj.ast.{MethodDefApi => PMethodDefApi}
import tiny.ast.UseTree


@component
trait MethodDefDefTyperComponent extends
    ooj.typechecker.MethodDefDefTyperComponent {
  (mthd: PMethodDefApi) => {
    mthd match {
      case mthd: MethodDefApi                =>
        val res1 = super.apply(mthd).asInstanceOf[ooj.ast.MethodDefApi]
        val throwsClause = mthd.throwsClause.map { tc =>
          typed(tc).asInstanceOf[UseTree]
        }
        val res2 = TreeUpgraders.upgradeMethodDef(res1)
        TreeCopiers.copyMethodDef(res2)(throwsClause = throwsClause)
      case mthd: PMethodDefApi               =>
        val res = TreeUpgraders.upgradeMethodDef(mthd)
        typed(res)
    }
  }
}
