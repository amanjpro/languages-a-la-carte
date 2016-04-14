package ch.usi.inf.l3.sana.modulej.classpath


import ch.usi.inf.l3.sana.tiny.ast.Tree
import scala.collection.immutable.SortedSet
import java.io.{File => JFile}



trait ClassPathCatalogApi {

  val classpath: List[JFile]
  val loader: ClassFileLoaderApi

  private[this] val sep = JFile.separator
  val binaryFileExtension: String = ".class"

  def trimExtension(fname: String): String = {
    val esize = binaryFileExtension.length
    val fsize = fname.length
    if(fsize > esize) {
      fname.substring(0, fsize - esize)
    } else ""
  }



  protected def makeCatalog(paths: List[JFile],
          acc: List[ClassPathInfo],
          parentPackages: List[String]): List[ClassPathInfo] = paths match {
    case Nil                                                =>
      acc
    case (x::xs) if x.isDirectory                           =>
      val pname = x.getName
      val members = makeCatalog(x.listFiles.toList, Nil,
        parentPackages ++ List(pname))
      val pinfo = PackageInfo(pname, parentPackages, members)
      makeCatalog(xs, acc ++ List(pinfo), parentPackages)
    case (x::xs) if x.getName.endsWith(binaryFileExtension) =>
      val cinfo = ClassInfo(trimExtension(x.getName),
        s"${x.getAbsolutePath}${sep}${x.getName}")
      makeCatalog(xs, acc ++ List(cinfo), parentPackages)
    case (x::xs)                                            =>
      makeCatalog(xs, acc, parentPackages)
  }



  val catalog: List[ClassPathInfo] = makeCatalog(classpath, Nil, Nil)



  def deinfes(fullyQualifiedName: String, isClass: Boolean = true): Boolean = {
    val names = fullyQualifiedName.split("[.]").toList
    defines(names, catalog, isClass)
  }

  private def defines(fullyQualifiedName: List[String],
        classpathInfo: List[ClassPathInfo],
        isClass: Boolean = true): Boolean = {
    def test(name: String, info: ClassPathInfo): Boolean =
      ((info.isInstanceOf[ClassInfo] && isClass) ||
        (info.isInstanceOf[PackageInfo] && !isClass)) && info.name == name

    fullyQualifiedName match {
      case Nil                                                  => false
      case (x::Nil)                                             =>
        classpathInfo.exists(i => test(x, i))
      case (x::xs)                                              =>
        classpathInfo.filter(i => test(x, i)).foldLeft(false)((z, y) => {
          y match {
            case pkg: PackageInfo if !z =>
              defines(xs, pkg.members, isClass)
            case _                      =>
              z
          }
        })
    }
  }


  def load(fullyQualifiedName: String): Option[Tree] = {
    val names = fullyQualifiedName.split("[.]").toList
    load(names, catalog)
  }

  private def load(fullyQualifiedName: List[String],
        classpathInfo: List[ClassPathInfo]): Option[Tree] = {
    def test(name: String, info: ClassPathInfo): Boolean =
      (info.isInstanceOf[ClassInfo] && info.name == name)

    fullyQualifiedName match {
      case Nil                                                  =>
        None
      case (x::Nil)                                             =>
        classpathInfo.find(i => test(x, i)).map {
          case cinfo: ClassInfo => loader.loadClass(cinfo.filepath)
        }
      case (x::xs)                                              =>
        val z: Option[Tree] = None
        classpathInfo.filter(i => test(x, i)).foldLeft(z)((z, y) => {
          y match {
            case pkg: PackageInfo if z != None =>
              load(xs, pkg.members)
            case _                             =>
              z
          }
        })
    }
  }



  trait ClassPathInfo {
    def name: String
  }

  case class PackageInfo(name: String,
    parentPackages: List[String],
    members: List[ClassPathInfo]) extends ClassPathInfo

  case class ClassInfo(name: String,
    filepath: String) extends ClassPathInfo

}


class ClassPathCatalog(val classpath: List[JFile],
  val loader: ClassFileLoaderApi) extends ClassPathCatalogApi
