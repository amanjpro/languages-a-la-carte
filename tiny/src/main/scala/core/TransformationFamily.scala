/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
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

package ch.usi.inf.l3.sana.tiny.core

/**
 * The supertype of all phase families
 */
trait PhaseFamily[P, R] {
  self =>

  /**
   * The default action of this family. This method is
   * applied whenever the family fails to find the proper
   * component for an input.
   */
  def default: PartialFunction[P, R] = ???

  /**
   * A reference to an instance of CompilerInterface. Using this reference
   * the families can access the basic functions of a compiler, like parser,
   * typer, and class-loader.
   */
  def compiler: CompilerInterface

  /**
   * The list of the components of this family
   */
  def components: List[PartialFunction[P, R]]

  /**
   * The family (delegate) method of this family. This search among all
   * the components and using their isDefinedAt method can find the right
   * component for the input, in case there is no suitable component, default
   * is applied.
   */
  def family: P => R = { p =>
    var comp = default
    val iter = components.iterator

    var shouldContinue = true
    while(iter.hasNext && shouldContinue) {
      val candidate = iter.next
      if(candidate.isDefinedAt(p)) {
        comp = candidate
        shouldContinue = false
      }
    }
    comp(p)
  }
}

/** The supertyp of all transformation families */
trait TransformationFamily[P, R] extends PhaseFamily[P, R]

/** The supertyp of all checker families */
trait CheckerFamily[P] extends PhaseFamily[P, Unit]
