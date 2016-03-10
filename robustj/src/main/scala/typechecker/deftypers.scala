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

import sana.core.TransformationComponent
import sana.dsl._
import robustj.ast.{TreeFactories, MethodDefApi}
import tiny.ast.UseTree


@component
trait MethodDefDefTyperComponent extends
    ooj.typechecker.MethodDefDefTyperComponent {
  (mthd: MethodDefApi) => {
    val res1 = super.apply(mthd).asInstanceOf[ooj.ast.MethodDefApi]
    val throwsClause = mthd.throwsClause.map { tc =>
      typed(tc).asInstanceOf[UseTree]
    }
    // INFO: a bit of hack, but works
    val res2 = TreeFactories.mkMethodDef(res1.mods, res1.ret, res1.name,
      res1.params, throwsClause, res1.body)
    res2.attributes = res1.attributes
    res2
  }
}
