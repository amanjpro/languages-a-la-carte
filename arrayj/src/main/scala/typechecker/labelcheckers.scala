package ch.usi.inf.l3.sana.arrayj.typechecker

import ch.usi.inf.l3.sana
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.dsl._

import arrayj.ast._
import brokenj.typechecker.LabelNameCheckerComponent






/*
ArrayCreation: DONE
ArrayAccess: DONE
ArrayTypeUse: DONE
ArrayInitializer: DONE
*/



@component(tree, labelNames)
trait ArrayCreationLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (creation: ArrayCreationApi) => {
    check((creation.array, labelNames))
    creation.size.foreach(size => check((size, labelNames)))
  }
}

@component(tree, labelNames)
trait ArrayAccessLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (access: ArrayAccessApi) => {
    check((access.array, labelNames))
    check((access.index, labelNames))
  }
}

@component(tree, labelNames)
trait ArrayTypeUseLabelNameCheckerComponent extends LabelNameCheckerComponent {
  (tuse: ArrayTypeUseApi) => {
    check((tuse.tpt, labelNames))
  }
}

@component(tree, labelNames)
trait ArrayInitializerLabelNameCheckerComponent
  extends LabelNameCheckerComponent {
  (init: ArrayInitializerApi) => {
    init.elements.foreach(elem => check((elem, labelNames)))
  }
}
