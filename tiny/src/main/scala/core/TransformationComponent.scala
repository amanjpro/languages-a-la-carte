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
 * Augments Function1 with two variants of join method.
 *
 * join method is basically `andThen` except when the return type
 * of the second partial function is Unit, in this case it returns
 * the return of the first function.
 */
object Implicits {
  implicit val dummy = ()

  implicit class Function1Component[P, R](component: P => R) {

    /**
     * The first variant of join, which is basically andThen
     */
    def join[A](other: R => A): P => A = component andThen other

    /**
     * The second variant of join. This returns the value of the first
     * partial function, after applying the second function.
     */
    def join(other: R => Unit)(implicit evidence: Unit): P => R = (p: P) => {
      val r = component(p)
      other(r)
      r
    }
  }


}


// trait MonadicComponent[P <: SyntaxComponent, R <: SyntaxComponent]
//     extends PhaseComponent[P, R] {
//   self =>
//
//   // def point[P, R](r: R): PhaseComponent[P, R]
//
//
// }


/**
 * The supertype of all phase components in Sana.
 */
trait PhaseComponent[P, R] extends PartialFunction[P, R] {
  self =>

  /** type aliases for the input */
  type Input  = P

  /** type aliases for the output */
  type Output = R

  /**
   * A reference to an instance of CompilerInterface. Using this reference
   * the components can access the basic functions of a compiler, like parser,
   * typer, and class-loader.
   */
  def compiler: CompilerInterface

  /**
   * In an attempt to make PhaseComponent monadic, we need to have a
   * unit function, in this case point is the unit function. It basically
   * creates the smallest possible partial function of the input.
   */
  def point(r: R): PhaseComponent[P, R] = new PhaseComponent[P, R] {
    def apply(p: P): R = r
    val compiler: CompilerInterface = self.compiler
    def isDefinedAt(p: P): Boolean = true
  }

  /**
   * Equivalent to the apply function of partial functions.
   */
  def run(p: P): R = apply(p)

  /**
   * The bind function of this monadic container (partial function).
   */
  def flatMap[T](other: R => PhaseComponent[P, T]): PhaseComponent[P, T] = {
    new PhaseComponent[P, T] {
      def apply(p: P): T = other(self(p)).apply(p)
      val compiler: CompilerInterface = self.compiler
      def isDefinedAt(p: P): Boolean = self.isDefinedAt(p)
    }
  }
}

/**
 * The supertype of all transformation components. A transformation component
 * is a component which might transform the input.
 */
trait TransformationComponent[P, R] extends PhaseComponent[P, R]

/**
 * The supertype of all checker components. A checker component
 * is a component which may not transform the input and performs a number of
 * checks..
 */
trait CheckerComponent[P] extends PhaseComponent[P, Unit]
