package ch.usi.inf.l3.sana.tiny.core

trait PhaseFamily[P, R] {
  self =>

  def default: PartialFunction[P, R] = ???
  def compiler: CompilerInterface

  def components: List[PartialFunction[P, R]]

  def family: P => R = components.reduce((x, y) => x orElse y) orElse default
}

trait TransformationFamily[P, R] extends PhaseFamily[P, R]

trait CheckerFamily[P] extends PhaseFamily[P, Unit]
