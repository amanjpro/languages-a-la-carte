package ch.usi.inf.l3.sana.tiny.ast


import ch.usi.inf.l3.sana
import sana.tiny.names.Name


trait TreeExtractors {


  trait TypeUseExtractor {
    def unapply(tuse: TypeUseApi): Option[Name] = tuse match {
      case null          => None
      case _             => Some(tuse.name)
    }
  }


  trait IdentExtractor {
    def unapply(id: IdentApi): Option[Name] = id match {
      case null          => None
      case _             => Some(id.name)
    }
  }
}


object TreeExtractors extends TreeExtractors {
  val TypeUse = new TypeUseExtractor {}
  val Ident   = new IdentExtractor {}
}
