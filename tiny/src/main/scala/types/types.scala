package ch.usi.inf.l3.sana.tiny


package types {

  trait Type {
    def =:=(other: Type): Boolean
    def =/=(other: Type): Boolean = !(this =:= other)
    def <:<(other: Type): Boolean
    def >:>(other: Type): Boolean = this =:= other || !(this <:< other)
  }

  object ErrorType extends Type {
    def =:=(other: Type): Boolean = false
    def <:<(other: Type): Boolean = false
  }
}


package object types {
  def tpeToString(tpe: Option[Type]): String =
    tpe.map(_.toString).getOrElse("<no type>")
}
