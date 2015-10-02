package ch.usi.inf.l3.sana.tiny.types



trait TypeUtils {
  def checkList(ts1: List[Type], ts2: List[Type],
    f: (Type, Type) => Boolean): Boolean = {
    if(ts1.size == ts2.size)
      ts1.zip(ts2).foldLeft(true)((z, y) => {
        z && f(y._1, y._2)
      })
    else false
  }
}

object TypeUtils extends TypeUtils
