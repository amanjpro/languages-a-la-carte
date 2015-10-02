package ch.usi.inf.l3.sana.core

trait PhaseFamily[P, R] {

  def default: R = ???

  def components: List[PartialFunction[P, R]]

  def family: P => R = components.reduce((x, y) => x orElse y) orElse {
    case _ => default
  }
}

trait TransformationFamily[P, R] extends PhaseFamily[P, R]

trait CheckerFamily[P] extends PhaseFamily[P, Unit]
