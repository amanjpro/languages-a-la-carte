package ch.usi.inf.l3.sana.arrayj.typechecker

import ch.usi.inf.l3.sana
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import sana.dsl._

import arrayj.ast._
import brokenj.typechecker.JumpCheckerComponent






/*
ArrayCreation: DONE
ArrayAccess: DONE
ArrayTypeUse: DONE
ArrayInitializer: DONE
*/



@component(tree, encls)
trait ArrayCreationJumpCheckerComponent extends JumpCheckerComponent {
  (creation: ArrayCreationApi) => {
    check((creation.array, encls))
    creation.size.foreach(size => check((size, encls)))
  }
}

@component(tree, encls)
trait ArrayAccessJumpCheckerComponent extends JumpCheckerComponent {
  (access: ArrayAccessApi) => {
    check((access.array, encls))
    check((access.index, encls))
  }
}

@component(tree, encls)
trait ArrayTypeUseJumpCheckerComponent extends JumpCheckerComponent {
  (tuse: ArrayTypeUseApi) => {
    check((tuse.tpt, encls))
  }
}

@component(tree, encls)
trait ArrayInitializerJumpCheckerComponent extends JumpCheckerComponent {
  (init: ArrayInitializerApi) => {
    init.elements.foreach(elem => check((elem, encls)))
  }
}
