package ch.usi.inf.l3.sana.robustj.namers

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
import tiny.ast.Implicits._
import robustj.symbols.MethodSymbol
import robustj.ast.{TreeFactories, MethodDefApi, TreeCopiers, TreeUpgraders}
import primj.ast.{MethodDefApi => PMethodDefApi}
import tiny.ast.UseTree

@component
trait MethodDefNamerComponent extends
    ooj.namers.MethodDefNamerComponent {
  (mthd: PMethodDefApi) => {
    mthd match {
      case mthd: MethodDefApi                  =>
        val res1 = super.apply(mthd).asInstanceOf[PMethodDefApi]
        val throwsClause = mthd.throwsClause.map { tc =>
          name(tc).asInstanceOf[UseTree]
        }
        res1.symbol.foreach {
          case mthd: MethodSymbol     =>
            mthd.throwsSymbols = throwsClause.flatMap(_.symbol)
          case _                      =>
            ()
        }
        // INFO: a bit of hack, but works
        val res2 = TreeUpgraders.upgradeMethodDef(res1)
        TreeCopiers.copyMethodDef(res2)(throwsClause = throwsClause)
      case mthd: PMethodDefApi                  =>
        val res = TreeUpgraders.upgradeMethodDef(mthd)
        name(res)
    }
  }
}
