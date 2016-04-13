package ch.usi.inf.l3.sana.tiny.core

object Implicits {
  implicit val dummy = ()

  implicit class Function1Component[P, R](component: P => R) {
    def join[A](other: R => A): P => A = (p: P) => {
      val r = component(p)
      other(r)
    }

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

trait PhaseComponent[P, R] extends PartialFunction[P, R] {
  self =>

  type Input  = P
  type Output = R

  def compiler: CompilerInterface

  def point(r: R): PhaseComponent[P, R] = new PhaseComponent[P, R] {
    def apply(p: P): R = r
    val compiler: CompilerInterface = self.compiler
    def isDefinedAt(p: P): Boolean = true
  }

  def run(p: P): R = apply(p)

  def flatMap[T](other: R => PhaseComponent[P, T]): PhaseComponent[P, T] = {
    new PhaseComponent[P, T] {
      def apply(p: P): T = other(self(p)).apply(p)
      val compiler: CompilerInterface = self.compiler
      def isDefinedAt(p: P): Boolean = self.isDefinedAt(p)
    }
  }
}

trait TransformationComponent[P, R] extends PhaseComponent[P, R]

trait CheckerComponent[P] extends PhaseComponent[P, Unit]
