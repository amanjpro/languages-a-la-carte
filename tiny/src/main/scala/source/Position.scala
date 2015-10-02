package ch.usi.inf.l3.sana.tiny.source


trait Position {
  def source: String
  def row: Int
  def col: Int
}




object Position {

  private class PosImpl(val source: String,
    val row: Int, val col: Int) extends Position {

    override def toString: String = s"${source}: line: ${row} - column: ${col}"
  }


  def apply(source: String, row: Int, col: Int): Position = {
    new PosImpl(source, row, col)
  }

  def unapply(p: Position): Option[(String, Int, Int)] = p match {
    case null => None
    case p    => Some((p.source, p.row, p.col))
  }
}

