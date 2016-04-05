package ch.usi.inf.l3.sana.modulej.classpath


import scala.collection.immutable.SortedSet
import java.io.{File => JFile}



trait ClassPathCatalogApi {

  val classpath: List[JFile]

  protected def makeCatalog(paths: List[JFile],
          acc: SortedSet[Path]): SortedSet[Path] =
    paths match {
      case Nil                                      =>
        acc
      case (x::xs) if x.isDirectory                 =>
        val kids = makeCatalog(x.listFiles.toList, SortedSet.empty[Path])
        val name = x.getName
        makeCatalog(xs, acc + new Directory(name, kids))
      case  (x::xs) if x.getName.endsWith(".class") =>
        makeCatalog(xs, acc + new File(x.getName))
      case  (x::xs)                                 =>
        makeCatalog(xs, acc)
    }

  val catalog: List[Bundle] =
    classpath.map((x) => {
      val res = makeCatalog(List(x), SortedSet.empty[Path])
      res.toList.head.toBundle
    })


  protected def toPath(path: List[String],
                     isClass: Boolean): Path = path match {
    case Nil                                            => ???
    case List(x)     if isClass                         =>
      new File(x + ".class")
    case List(x)                                        =>
      new Directory(x, SortedSet.empty[Path])
    case (x::xs)                                        =>
      val rest = toPath(xs, isClass)
      new Directory(x, SortedSet(rest))
  }

  def defines(fullName: String, isClass: Boolean): Boolean = {
    val path = toPath(fullName.split("[.]").toList, isClass)
    val r = catalog.foldLeft(false)((z, y) => {
      z || y.path.foldLeft(false)((z, y) => z || y.defines(path))
    })
    r
  }

  case class Bundle(val path: SortedSet[Path])
  sealed trait Path {
    def defines(that: Path): Boolean
    def toBundle: Bundle = this match {
      case f: File        => Bundle(SortedSet(f))
      case d: Directory   => Bundle(d.children)
    }
  }

  protected object Path {
    implicit def ordering[A <: Path]: Ordering[A] = new Ordering[A] {
      override def compare(x: A, y: A): Int = (x, y) match {
        case (f: File, s: File)               => f.name.compareTo(s.name)
        case (_: File, _)                     => 1
        case (_, _: File)                     => -1
        case (f: Directory, s: Directory)     => f.name.compareTo(s.name)
      }
    }
  }




  protected class File(val name: String) extends Path {
    override def equals(other: Any): Boolean = other match {
      case f: File          =>
        f.name == name
      case _                =>
        false
    }

    def defines(that: Path): Boolean = this == that

    override def hashCode: Int = name.hashCode
    override def toString: String = s"File(name = $name)"
  }

  protected class Directory(val name: String,
    val children: SortedSet[Path]) extends Path {
    override def toString: String = {
      val kids = children.mkString(",")
      s"Directory(name = $name, children = {$kids})"
    }

    def defines(that: Path): Boolean = that match {
      case that: Directory if this == that             =>
        children.foldLeft(false){
          (z, y) => {
            val kids = that.children.toList
            z || (if (kids == Nil) true else y.defines(kids.head))
          }
        }
      case _                                           =>
        false
    }
    override def equals(other: Any): Boolean = other match {
      case f: Directory          =>
        f.name == name
      case _                     =>
        false
    }
    override def hashCode: Int = name.hashCode
  }
}


class ClassPathCatalog(val classpath: List[JFile]) extends ClassPathCatalogApi
