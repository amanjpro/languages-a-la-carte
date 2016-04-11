package ch.usi.inf.l3.sana.modulej.classpath

import ch.usi.inf.l3.sana
import sana.modulej
import sana.ppj
import sana.dynj
import sana.robustj
import sana.arrooj
import sana.arrayj
import sana.ooj
import sana.primj
import sana.tiny
import sana.calcj


import tiny.ast.Tree
import tiny.names.Name
import ooj.names.StdNames.DEFAULT_PACKAGE_NAME
import ooj.ast.Implicits._
import ooj.ast.ClassDefApi
import modulej.ast.TreeFactories

import scala.collection.immutable.SortedSet
import java.io.{File => JFile}



trait ClassPathLoaderApi {
  val classpath: List[JFile]
  lazy val classLoader: ClassFileLoaderApi = new ClassFileLoader(classpath)
  private[this] val sep = JFile.separator

  def loadClasspath: List[Tree] = {
    val loaded = loadClasses(classpath, Nil, Nil)
    val (clazzes, pkgs) = loaded.partition(_.isInstanceOf[ClassDefApi])
    val units    = {
      val compilationUnits = groupUnits(clazzes)
      for {
        cu   <- compilationUnits
        cdef <- cu.headOption
      } yield {
        val (sname, spath) = {
          val cdef2 = cdef.asInstanceOf[ClassDefApi]
          val name = cdef2.sourceName
          name.map(_.split("/").toList) match {
            case Some((x::xs))     => (x, xs)
            case _                 => ("", Nil)
          }
        }
        val pkg = TreeFactories.mkPackageDef(Nil,
          DEFAULT_PACKAGE_NAME, cu)
        TreeFactories.mkCompilationUnit(pkg, sname, spath)
      }
    }
    units ++ pkgs
  }

  private[this] def loadClasses(paths: List[JFile],
    acc: List[Tree],
    containingPackages: List[Name]): List[Tree] = paths match {
    case p::ps   if p.isFile                   =>
      val clazz =
        classLoader.loadClass(s"${p.getAbsolutePath}$sep${p.getName}")
      loadClasses(ps, clazz::acc, containingPackages)
    case p::ps                                 =>
      val pName = Name(p.getName)
      val members  = loadClasses(p.listFiles.toList, Nil,
        containingPackages ++ List(pName))
      val units    = {
        val compilationUnits = groupUnits(members)
        for {
          cu   <- compilationUnits
          cdef <- cu.headOption
        } yield {
          val (sname, spath) = {
            val cdef2 = cdef.asInstanceOf[ClassDefApi]
            val name = cdef2.sourceName
            name.map(_.split("/").toList) match {
              case Some((x::xs))     => (x, xs)
              case _                 => ("", Nil)
            }
          }
          val pkg = TreeFactories.mkPackageDef(containingPackages,
            pName, cu)
          TreeFactories.mkCompilationUnit(pkg, sname, spath)
        }
      }
      loadClasses(ps, units ++ acc, containingPackages)
    case Nil                                   =>
      acc
  }


  def groupUnits(members: List[Tree]): List[List[Tree]] = {
    val (clazzes, others) = members.partition(_.isInstanceOf[ClassDefApi])
    val clazzes2 =
      clazzes
        .map(_.asInstanceOf[ClassDefApi])
        .groupBy(_.sourceName)
        .map(_._2).toList
    others::clazzes2
  }
}


class ClassPathLoader(val classpath: List[JFile]) extends ClassPathLoaderApi
