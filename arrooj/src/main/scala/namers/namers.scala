package ch.usi.inf.l3.sana.arrooj.namers

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.calcj
import sana.brokenj
import sana.arrayj
import sana.arrooj

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast.{TreeCopiers => _, _}
import arrooj.ast.Implicits._
import tiny.symbols._
import arrayj.ast.ArrayTypeUseApi
import arrooj.symbols.SymbolUtils

@component
trait ArrayTypeUseNamerComponent
    extends arrayj.namers.ArrayTypeUseNamerComponent {
  (tuse: ArrayTypeUseApi)     => {
    val res = super.apply(tuse).asInstanceOf[ArrayTypeUseApi]
    res.tpt.symbol.foreach { sym =>
      res.symbol = SymbolUtils.mkArraySymbol(sym)
    }
    res
  }
}
