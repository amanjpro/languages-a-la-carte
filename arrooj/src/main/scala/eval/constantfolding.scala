package ch.usi.inf.l3.sana.arrooj.eval

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


import ooj.eval.ConstantFoldingComponent
import arrayj.ast._
import tiny.ast.{UseTree, Expr}
import arrooj.ast.Implicits._
import arrooj.types.TypeUtils
import arrooj.symbols.SymbolUtils

/*
ArrayTypeUse: DONE
ArrayCreation: DONE
ArrayInitializer: DONE
ArrayAccess: DONE
*/
@component(tree, env)
trait ArrayTypeUseConstantFoldingComponent
  extends ConstantFoldingComponent {
  (tuse: ArrayTypeUseApi) => {
    val (tpt, env1) = constantFold((tuse.tpt, env))
    val res =
      TreeCopiers.copyArrayTypeUse(tuse)(tpt = tpt.asInstanceOf[UseTree])
    tpt.tpe.foreach { tpe =>
      res.tpe = TypeUtils.mkArrayType(tpe)
    }
    tpt.symbol.foreach { sym =>
      res.symbol = SymbolUtils.mkArraySymbol(sym)
    }
    (res, env1)
  }
}

@component(tree, env)
trait ArrayCreationConstantFoldingComponent
  extends ConstantFoldingComponent {
  (creation: ArrayCreationApi) => {
    val (array, env1) = constantFold((creation.array, env))
    creation.size map { size =>
      val (size2, env2) = constantFold((size, env1))
      val res =
        TreeCopiers.copyArrayCreation(creation)(
          array = array.asInstanceOf[Expr],
          size = Some(size2.asInstanceOf[Expr]))
      (res, env2)
    } getOrElse {
        val res = TreeCopiers.copyArrayCreation(creation)(
          array = array.asInstanceOf[Expr])
        (res, env1)
    }
  }
}


@component(tree, env)
trait ArrayInitializerConstantFoldingComponent
  extends ConstantFoldingComponent {
  (init: ArrayInitializerApi) => {
    val zero: List[Expr] = Nil
    val (stnemele, env1) = init.elements.foldLeft((zero, env))((z, y) => {
      val zelements = z._1
      val zenv      = z._2
      val (y1, y2)  = constantFold((y, zenv))
      ((y1.asInstanceOf[Expr]::zelements, y2))
    })
    val res =
      TreeCopiers.copyArrayInitizalizer(init)(elements = stnemele.reverse)
    (res, env1)
  }
}

@component(tree, env)
trait ArrayAccessConstantFoldingComponent
  extends ConstantFoldingComponent {
  (access: ArrayAccessApi) => {
    val (array, env1) = constantFold((access.array, env))
    val (index, env2) = constantFold((access.index, env1))
    val res =
      TreeCopiers.copyArrayAccess(access)(array = array.asInstanceOf[Expr],
        index = index.asInstanceOf[Expr])
    (res, env1)
  }
}
