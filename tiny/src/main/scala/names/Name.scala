package ch.usi.inf.l3.sana.tiny.names

class Name(val asString: String) extends AnyVal {
  override def toString: String = asString
}

object Name {
  def apply(name: String): Name = new Name(name)
}

