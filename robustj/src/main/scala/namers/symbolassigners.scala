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

import sana.core.TransformationComponent
import sana.dsl._
import tiny.ast.{TreeCopiers => _, TreeFactories => _, _}
import arrooj.ast.Implicits._
import tiny.symbols._
import robustj.modifiers._
import robustj.ast._
import robustj.symbols._
import primj.namers.SymbolAssignerComponent
import primj.symbols.ScopeSymbol
import primj.ast.{ValDefApi, BlockApi}

@component
trait MethodDefSymbolAssignerComponent
    extends ooj.namers.MethodDefSymbolAssignerComponent {
  (mthd: MethodDefApi) => {
    val res1 = super.apply(mthd).asInstanceOf[ooj.ast.MethodDefApi]
    val throwsClause = mthd.throwsClause.map { tc =>
      mthd.symbol.foreach(tc.owner = _)
      assign(tc).asInstanceOf[UseTree]
    }
    val res2         = TreeFactories.mkMethodDef(mthd.mods,
                                                  mthd.ret,
                                                  mthd.name,
                                                  mthd.params,
                                                  throwsClause,
                                                  mthd.body)
    res2.attributes = res1.attributes
    res2
  }
  override protected def createMethodSymbol(mthd: ooj.ast.MethodDefApi,
    owner: Option[Symbol]): primj.symbols.MethodSymbol =
    MethodSymbol(mthd.mods, mthd.name, None, Nil, Nil, None, owner)
}


@component trait TrySymbolAssignerComponent
  extends SymbolAssignerComponent {
  (tri: TryApi) => {
    tri.owner.foreach { sym =>
      tri.tryClause.owner = sym
      tri.catches.foreach(_.owner = sym)
      tri.finallyClause.foreach(_.owner = sym)
    }
    val tryClause     = assign(tri.tryClause).asInstanceOf[BlockApi]
    val catches       = tri.catches.map { ctch =>
      assign(ctch).asInstanceOf[CatchApi]
    }
    val finallyClause = tri.finallyClause.map { fc =>
      assign(fc).asInstanceOf[BlockApi]
    }

    TreeCopiers.copyTry(tri)(tryClause =tryClause,
      catches = catches, finallyClause = finallyClause)
  }
}

@component trait ThrowSymbolAssignerComponent
  extends SymbolAssignerComponent {
  (thrw: ThrowApi) => {
    thrw.owner.foreach { sym =>
      thrw.expr.owner = sym
    }
    val expr     = assign(thrw.expr).asInstanceOf[Expr]

    TreeCopiers.copyThrow(thrw)(expr = expr)
  }
}

@component trait CatchSymbolAssignerComponent
  extends SymbolAssignerComponent {
  (ctch: CatchApi) => {
    val symbol  = {
      val temp = ScopeSymbol(ctch.owner)
      temp.mods = temp.mods | CATCH_SYMBOL
      temp
    }
    ctch.eparam.owner      = symbol
    ctch.catchClause.owner = symbol
    ctch.symbol            = symbol
    val eparam             = assign(ctch.eparam).asInstanceOf[ValDefApi]
    val catchClause        = assign(ctch.catchClause).asInstanceOf[BlockApi]
    TreeCopiers.copyCatch(ctch)(eparam = eparam, catchClause = catchClause)
  }
}


