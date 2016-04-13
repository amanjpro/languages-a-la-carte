package ch.usi.inf.l3.sana.tiny.core

trait LanguageModule[P, R] {
  def compile: P => R
}
