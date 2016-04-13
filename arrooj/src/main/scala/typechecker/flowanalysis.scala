package ch.usi.inf.l3.sana.arrooj.typechecker


import ch.usi.inf.l3.sana
import sana.arrooj
import sana.arrayj
import sana.ooj
import sana.brokenj
import sana.primj
import sana.calcj
import sana.tiny


import tiny.dsl._
import tiny.core._
import ooj.typechecker.{N, FlowCorrectnessCheckerComponent}
import arrayj.ast.{ArrayCreationApi, ArrayAccessApi,
                   ArrayTypeUseApi, ArrayInitializerApi}

/*
ArrayCreation: DONE
ArrayAccess: DONE
ArrayInitializer: DONE
ArrayTypeUse: DONE
*/

@component(tree, env)
trait ArrayCreationFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (creation: ArrayCreationApi) => {
    check((creation.array, env))
    creation.size.foreach(s => check((s, env)))
    N
  }
}


@component(tree, env)
trait ArrayAccessFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (access: ArrayAccessApi) => {
    check((access.array, env))
    check((access.index, env))
    N
  }
}

@component(tree, env)
trait ArrayInitializerFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (init: ArrayInitializerApi) => {
    init.elements.foreach(elem => check((elem, env)))
    N
  }
}

@component(tree, env)
trait ArrayTypeUseFlowCorrectnessCheckerComponent extends
  FlowCorrectnessCheckerComponent {
  (tuse: ArrayTypeUseApi) => {
    check((tuse.tpt, env))
    N
  }
}
