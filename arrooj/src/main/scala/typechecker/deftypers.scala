package ch.usi.inf.l3.sana.arrooj.typechecker


import ch.usi.inf.l3.sana
import sana.arrooj
import sana.arrayj
import sana.ooj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.core.TransformationComponent
import sana.dsl._


import arrooj.types._
import arrooj.symbols.SymbolUtils
import arrooj.types.TypeUtils
import arrayj.ast.{TreeUtils => _, TreeCopiers => _, _}
import arrooj.ast._
import arrooj.ast.Implicits._
import ooj.typechecker.DefTyperComponent
import tiny.types.Type
import tiny.ast.{UseTree, Tree}


/*
ArrayTypeUse: DONE
*/


@component
trait ArrayTypeUseDefTyperComponent extends DefTyperComponent {
  (tuse: ArrayTypeUseApi) => {
    val tpt = typed(tuse.tpt).asInstanceOf[UseTree]
    tpt.symbol.foreach { sym =>
      tuse.symbol = SymbolUtils.mkArraySymbol(sym)
    }
    tpt.tpe.foreach { tpe =>
      tuse.tpe = TypeUtils.mkArrayType(tpe)
    }
    TreeCopiers.copyArrayTypeUse(tuse)(tpt = tpt)
  }
}

@component
trait SelectDefTyperComponent
  extends ooj.typechecker.SelectDefTyperComponent {
  override protected def isTypeUse(tree: Tree): Boolean = tree match {
    case t: UseTree => TreeUtils.isTypeUse(t)
    case _          => false
  }
}
