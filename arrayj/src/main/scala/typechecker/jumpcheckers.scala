/*
 * Copyright (c) <2015-2016>, see CONTRIBUTERS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package ch.usi.inf.l3.sana.arrayj.typechecker

import ch.usi.inf.l3.sana
import sana.arrayj
import sana.brokenj
import sana.primj
import sana.tiny
import sana.calcj

import tiny.dsl._

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
