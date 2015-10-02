package ch.usi.inf.l3.sana.core

trait LanguageModule[P, R] {
  def compile: P => R
}
