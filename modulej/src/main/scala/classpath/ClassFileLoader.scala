/*
 * Copyright (c) <2015-2016>, see CONTRIBUTORS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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

import tiny.names.Name
import tiny.symbols._

import tiny.ast.{TreeFactories => _, _}
import calcj.ast.{TreeFactories => _, _}
import primj.ast.{TreeFactories => _, ProgramApi => _, MethodDefApi => _, _}
import arrayj.ast.{TreeFactories => _, _}
import ooj.ast.{TreeFactories => _, MethodDefApi => _, _}
import robustj.ast.{TreeFactories => _, _}
import ppj.ast.{TreeFactories => _, _}
import modulej.ast._

import tiny.types._
import calcj.types._
import primj.types._
import ooj.types._

import tiny.modifiers._
import primj.modifiers._
import ooj.modifiers._
import robustj.modifiers._
import ppj.modifiers._
import modulej.modifiers._

import ppj.modifiers.Ops._
import robustj.names.StdNames._
import modulej.ast.TreeExtractors._
import ooj.ast.Implicits._


import org.objectweb.asm._
import java.lang.{ClassLoader, Class => JClass}
import java.net.{URL, URI, URLClassLoader, URLEncoder}
import java.io.{IOException, ByteArrayOutputStream,
                BufferedInputStream, InputStream, File => JFile}

import scala.collection.mutable

/**
 * This trait is used to load and parse class files from the classpath.
 */
trait ClassPathLoaderApi {

  /** The list of all available classpaths, as a list of {{{java.lang.File}}} */
  protected def classPaths: List[JFile]

  /** The list of all available classpaths, as a list of {{{java.net.URL}} */
  private[this] lazy val urls: Array[URL] =
    classPaths.map(_.toURI.toURL).toArray

  /** An instance of class-file loader */
  private[this] lazy val classLoader: SanaClassLoader =
    new SanaClassLoader(urls)

  type Path     = String
  type FullName = String

  /** A list of all loaded classes, we keep track of them not to load a class twice */
  private[this] var loadedClasses: List[FullName] = Nil

  /** The platform dependent file separator */
  private[this] val sep = JFile.separator

  /**
   * The language (and platform) dependent class file extension, `.class` in
   * terms of JVM
   */
  val binaryFileExtension: String = ".class"


  /**
   * Checks if a class or a package with a given fully qualified name is in
   * in the claspath.
   *
   * @param fullyQualifiedName the fully qualified name to be searched for
   * @param isClass true if looking for a class or interface, false if looking
   *                for a package
   */
  def defines(fullyQualifiedName: FullName, isClass: Boolean): Boolean = {
    val fname = fullyQualifiedName.replaceAll("[.]", "/")
    classPaths.foldLeft(false)((z, y) => {
      if(!z) {
        findPath(fullyQualifiedName, isClass).map(_ => true).getOrElse(false)
      } else z
    })
  }

  /**
   * Loads a class and all its dependencies, in case if it is not already loaded.
   * Should it be already loaded, return the already loaded one.
   *
   * @param fullyQualifiedName the fully qualified name to be searched for
   */
  def load(fullyQualifiedName: FullName): Option[Tree] = {
    val cname = findPath(fullyQualifiedName, true)
    val res = cname.map(loadClass(_,
      fullyQualifiedName.split("[.]").toList.dropRight(1),
      fullyQualifiedName))
    res.map(TreeFactories.mkProgram(_))
  }

  /**
   * Finds a path of a class/interface or package in the classpath.
   *
   * @param fullyQualifiedName the fully qualified name to be searched for
   * @param isClass true if looking for a class or interface, false if looking
   *                for a package
   */
  protected def findPath(fullyQualifiedName: FullName,
          isClass: Boolean): Option[Path] = {
    val z: Option[String] = None
    val fname = fullyQualifiedName.replaceAll("[.]", "/")
    classPaths.foldLeft(z)((z, y) => {
      z match {
        case  None =>
          val n = s"${y}${sep}${fname}"//"${binaryFileExtension}"
          val splitter = if(sep == "\\") "\\\\" else "/"
          val fpath    = n.split(s"[$splitter]")
          val name     = fpath.last
          val path     = fpath.dropRight(1)
          val file    = new JFile(path.mkString(sep))
          file.listFiles match {
            case null              => None
            case files             =>
              files.flatMap ( f =>
                if(f.getName == s"${name}${binaryFileExtension}" &&
                  f.isFile && isClass) List(f.getCanonicalPath)
                else if(f.getName == s"${name}" &&
                  f.isDirectory && !isClass) List(f.getCanonicalPath)
                else Nil
              ).headOption
          }
          // if(f.exists && f.isFile) {
          //   Some(f.getCanonicalPath)
          // } else None
        case _     =>
          z
      }
    })
  }

  /**
   * Loads a class and all its dependencies, in case if it is not already loaded.
   * Should it be already loaded, return the already loaded one.
   *
   * @param url the path to the directory that contains the class
   * @param pkgs the list of all enclosing packages of the class
   * @param fullName the fully qualified name of the class
   */
  protected def loadClass(url: Path, pkgs: List[String],
          fullName: FullName): List[Tree] = {
    val (res, queue)      = loadClassAux(url, pkgs, fullName)
    val queuedClasses     = queue.foldLeft(Nil: List[Tree])((z, y) => {
      if(!loadedClasses.contains(y)) {
        findPath(y, true) match {
          case Some(cname) =>
            loadClass(cname,
              y.split("[.]").toList.dropRight(1), y) ++ z
          case None        =>
            z
        }
      } else z
    })
    res.toList ++ queuedClasses
  }

  /**
   * Loads a class and all its dependencies, in case if it is not already
   * loaded. Should it be already loaded, return the already loaded one.
   * A long side this class in question, this method also returns the list
   * of all classes that need to be loaded (the ones that the class in question
   * depends on and not loaded yet).
   *
   * @param url the path to the directory that contains the class
   * @param pkgs the list of all enclosing packages of the class
   * @param fullName the fully qualified name of the class
   */
  protected def loadClassAux(url: Path,
          pkgs: List[String],
          fullName: FullName): (Option[Tree], List[FullName]) = {
    if(!loadedClasses.contains(fullName)) {
      loadedClasses    = fullName :: loadedClasses
      val classData    = classLoader.getResourceAsStream(url)
      val cr           = new ClassReader(classData)
      val reader       = new ClassFileParser(fullName)
      cr.accept(reader, 0)
      val queue1       = reader.queue
      reader.queue = Nil
      val (innerClasses, queue2) = {
        val temp = reader.innerClasses
          .map ( ic => {
            val name = fullName + "$" + ic
            val url  = findPath(name, true)
            url match {
              case Some(url)      =>
                loadClassAux(url, pkgs, name)
              case None           =>
                (None, Nil)
            }
          })
        val z:(List[Tree], List[FullName]) = (Nil, Nil)
        temp.foldLeft(z)((z, y) => {
          y._1 match {
            case None           =>
              (z._1, z._2 ++ y._2)
            case Some(y1)       =>
              (y1::z._1, z._2 ++ y._2)
          }
        })
      }
      val body         =
        TreeFactories.mkTemplate(innerClasses ++ reader.clazz.body.members)
      val clazz        =
        TreeFactories.mkClassDef(reader.clazz.mods | COMPILED,
          reader.clazz.name, reader.clazz.parents, body)
      clazz.sourceName = reader.source
      def toPackages(pkgs: List[String], member: Tree,
                acc: List[Name]): Tree = pkgs match {
        case Nil                             => member
        case (x::xs)                         =>
          val rest = toPackages(xs, member, acc ++ List(Name(x)))
          TreeFactories.mkPackageDef(acc, Name(x), List(rest))
      }
      (Some(toPackages(pkgs, clazz, Nil)), queue1 ++ queue2)
    } else (None, Nil)
  }


  /**
   * A class to load class-files
   *
   * @param classpath the classpath of this class-loader
   */
  class SanaClassLoader(classpath: Array[URL])
    extends URLClassLoader(classpath, null) {

    /** the list of all loaded classes by this class-loader */
    private[this] val classes: mutable.Map[String, JClass[_]] =
      mutable.Map.empty

    override protected def findClass(name: String): JClass[_] =
      classes.get(name) match {
        case Some(cl)         => cl
        case None             =>
          val classData: Array[Byte] = try {
            loadClassData(name)
          } catch {
            case e: IOException =>
              throw new ClassNotFoundException(
                s"Class $name could not be found", e)
          }
          val c: JClass[_] = defineClass(name, classData, 0, classData.length)
          resolveClass(c)
          classes + (name -> c)
          c
      }


    override def getResourceAsStream(name: String): InputStream =
      new java.io.FileInputStream(name)

    /**
     * Load a class as an array of byte
     *
     * @param name the fully qualified name of the class
     */
    private def loadClassData(name: String): Array[Byte] = {
      val in: BufferedInputStream =
        new BufferedInputStream(getResourceAsStream(name))

      val out: ByteArrayOutputStream = new ByteArrayOutputStream()
      var i: Int = in.read

      while (i != -1) {
        out.write(i)
        i = in.read
      }

      in.close()
      val classData: Array[Byte] = out.toByteArray()
      out.close()
      classData
    }

    override def loadClass(className: String, resolveIt: Boolean): JClass[_] = {
      // Do not fallback to JVM's ClassLoader
      /* Check our local cache of classes */
      classes.get(className) match {
        case None            =>
          findClass(className)
        case Some(cl)        =>
          cl
      }
    }
  }

  /**
   * A class file parser
   *
   * @param version the version of ASM
   * @param className the class name to load
   */
  protected class ClassFileParser private(version: Int,
    val className: String) extends ClassVisitor(version) with Opcodes {

    /**
     * Instantiates a class file parser using ASM4 version
     *
     * @param className the class name to load
     */
    def this(className: String) = this(Opcodes.ASM4, className)

    /** The class name in the bytecode format */
    private[this] val bytecodeClassName = {
      className.replaceAll("[.]", "/")
    }

    /** The class name in the bytecode format */
    private[this] val simpleClassName   = bytecodeClassName
      // className.split("[.]").toList.last
    // }

    var clazz: ClassDefApi = _
    var clazzFactory: TemplateApi => ClassDefApi = _

    var members: List[DefTree] = Nil
    var source: String = _
    var innerClasses: List[String] = Nil
    var queue: List[FullName] = Nil

    protected def chopOneParam(paramString: String): (String, String) = {
      if(paramString.startsWith("B") ||
         paramString.startsWith("C") ||
         paramString.startsWith("S") ||
         paramString.startsWith("I") ||
         paramString.startsWith("J") ||
         paramString.startsWith("F") ||
         paramString.startsWith("D") ||
         paramString.startsWith("V") ||
         paramString.startsWith("Z")) {

        val rest  = paramString.substring(1)
        val param = paramString(0).toString
        (param, rest)
      } else if(paramString.startsWith("[")) {
        val (fst, snd) = chopOneParam(paramString.substring(1))
        ("[" + fst, snd)
      } else {
        val endIndex = paramString.indexOf(';') + 1
        paramString.splitAt(endIndex)
      }
    }
    protected def chopParams(paramString: String,
      acc: List[String]): List[String] = {
      if(paramString == "") acc
      else {
        val (fst, snd) = chopOneParam(paramString)
        chopParams(snd, fst::acc)
      }
    }

    protected def methodParams(paramString: String): List[ValDefApi] = {
      val params = chopParams(paramString, Nil).reverse

      params.zipWithIndex.map {
        case (paramTpe, index) =>
          TreeFactories.mkValDef(PARAM | COMPILED,
            stringToUseTree(paramTpe, false), Name("x" + index), NoTree)
      }
    }

    protected def stringToUseTree(sig: String,
                  classSig: Boolean): UseTree = {
      sig match {
        case "B"                        =>
          TreeFactories.mkTypeUse(BYTE_TYPE_NAME)
        case "C"                        =>
          TreeFactories.mkTypeUse(CHAR_TYPE_NAME)
        case "S"                        =>
          TreeFactories.mkTypeUse(SHORT_TYPE_NAME)
        case "I"                        =>
          TreeFactories.mkTypeUse(INT_TYPE_NAME)
        case "J"                        =>
          TreeFactories.mkTypeUse(LONG_TYPE_NAME)
        case "F"                        =>
          TreeFactories.mkTypeUse(FLOAT_TYPE_NAME)
        case "D"                        =>
          TreeFactories.mkTypeUse(DOUBLE_TYPE_NAME)
        case "Z"                        =>
          TreeFactories.mkTypeUse(BOOLEAN_TYPE_NAME)
        case "V"                        =>
          TreeFactories.mkTypeUse(VOID_TYPE_NAME)
        case nme if nme.startsWith("[") =>
          val last        = nme.lastIndexOf("[")
          val (dims, tpe) = nme.splitAt(last + 1)
          val tuse        = stringToUseTree(tpe, classSig)
          dims.foldLeft(tuse: UseTree)((z, y) => {
            TreeFactories.mkArrayTypeUse(z)
          })
        case nme        =>
          val sig2 = if(classSig) nme
                     else nme.substring(1, nme.size -1)
          queue = sig2.replaceAll("[/]", ".")::queue
          val use = stringToUseTree(sig2.split("/").toList)
          val t = use match {
            case select@Select(qual, id: Ident)            =>
              val tuse = TreeFactories.mkTypeUse(id.name)
              TreeFactories.mkSelect(qual, tuse)
            case id@Ident(name)                            =>
              TreeFactories.mkTypeUse(name)
            case use                                       =>
              use
          }
          t
      }
    }

    private def stringToUseTree(sig: List[String]): UseTree = sig match {
      case List(x)                =>
        TreeFactories.mkIdent(Name(x))
      case Nil                    =>
        throw new Exception("This should not happen")
      case xs                     =>
        val rest = stringToUseTree(xs.take(xs.size - 1))
        TreeFactories.mkSelect(
          rest, TreeFactories.mkIdent(Name(xs.last)))
    }


    protected def parseAccessFlag(access: Int): Flags = {
      if(hasFlag(access, Opcodes.ACC_PUBLIC))         Flags(PUBLIC_ACC)
      else if(hasFlag(access, Opcodes.ACC_PROTECTED)) Flags(PROTECTED_ACC)
      else if(hasFlag(access, Opcodes.ACC_PRIVATE))   Flags(PRIVATE_ACC)
      else                                            Flags(PACKAGE_ACC)
    }


    protected def parseNativeFlag(access: Int): Flags = {
      if(hasFlag(access, Opcodes.ACC_NATIVE))     Flags(NATIVE)
      else                                        noflags
    }
    protected def parseFinalFlag(access: Int): Flags = {
      if(hasFlag(access, Opcodes.ACC_FINAL))      Flags(FINAL)
      else                                        noflags
    }


    protected def parseInterfaceFlag(access: Int): Flags = {
      if(hasFlag(access, Opcodes.ACC_INTERFACE))  Flags(INTERFACE)
      else                                        noflags
    }

    protected def parseAbstractFlag(access: Int): Flags = {
      if(hasFlag(access, Opcodes.ACC_ABSTRACT))   Flags(ABSTRACT)
      else                                        noflags
    }

    protected def parseStaticFlag(access: Int): Flags = {
      if(hasFlag(access, Opcodes.ACC_STATIC))    Flags(STATIC)
      else                                       noflags
    }

    protected def hasFlag(allFlags: Int,
        flag: Int): Boolean = (allFlags & flag) == flag


    override def visit(version: Int, access: Int, name: String,
     signature: String, superName: String, interfaces: Array[String]): Unit = {

      val acc           = parseAccessFlag(access)
      val isFinal       = parseFinalFlag(access)
      val isInterface   = parseInterfaceFlag(access)
      val isAbstract    = parseAbstractFlag(access)
      val mods          = acc | isFinal | isInterface | isAbstract | COMPILED



      val parents = {
        val intfs = interfaces.toList.map(stringToUseTree(_, true))
        if(superName == null)
          intfs
        else {
          val sup = stringToUseTree(superName, true)
          sup::intfs
        }
      }

      clazzFactory = (body: TemplateApi) => {
        val simpleName = name.split("/").last
        TreeFactories.mkClassDef(mods, Name(simpleName), parents, body)
      }
    }

    override def visitSource(source: String, debug: String): Unit = {
      this.source = source
    }

    override def visitOuterClass(owner: String, name: String,
            desc: String): Unit = {

    }

    override def visitAnnotation(desc: String,
            visible: Boolean): AnnotationVisitor =  {
      null
    }

    override def visitAttribute(attr: Attribute): Unit = {

    }

    override def visitInnerClass(name: String, outerName: String,
            innerName: String, access: Int): Unit = {
      val acc   = parseAccessFlag(access)
      val sName = innerName.replaceAll("[/]", ".")
      if(sName == bytecodeClassName)
        innerClasses = sName::innerClasses
    }


    override def visitField(access: Int, name: String, desc: String,
          signature: String, value: Object): FieldVisitor = {
      val acc = parseAccessFlag(access)
      val isFinal = parseFinalFlag(access)
      val isStatic = parseStaticFlag(access)
      val mods = acc | isFinal | isStatic | FIELD | COMPILED


      val tpt  = stringToUseTree(desc, false)
      val rhs  = value match {
        case i: java.lang.Integer         =>
          TreeFactories.mkLiteral(IntConstant(i))
        case l: java.lang.Long            =>
          TreeFactories.mkLiteral(LongConstant(l))
        case f: java.lang.Float           =>
          TreeFactories.mkLiteral(FloatConstant(f))
        case d: java.lang.Double          =>
          TreeFactories.mkLiteral(DoubleConstant(d))
        case s: String                    =>
          TreeFactories.mkLiteral(StringConstant(s))
        case _                            =>
          NoTree
      }
      val vdef =
        TreeFactories.mkValDef(mods, tpt, Name(name), rhs)
      members = vdef::members
      null
    }

    override def visitMethod(access: Int, name: String, desc: String,
          signature: String, exceptions: Array[String]): MethodVisitor = {

      val acc           = parseAccessFlag(access)
      val isFinal       = parseFinalFlag(access)
      val isStatic      = parseStaticFlag(access)
      val isAbstract    = parseAbstractFlag(access)
      val isNative      = parseAbstractFlag(access)
      val isConstructor = if(name == CONSTRUCTOR_NAME.asString) {
        Flags(CONSTRUCTOR)
      } else noflags
      val mods          =
        acc | isFinal | isStatic | isAbstract | isConstructor | COMPILED


      val (paramString, retString) = {
        val endParamIndex = desc.indexOf(')')
        val (fst, snd)    = desc.splitAt(endParamIndex)
        if(name == CONSTRUCTOR_NAME.asString)
          (fst.substring(1), s"L$simpleClassName;")
        else
          (fst.substring(1), snd.substring(1))
      }

      val ret    = stringToUseTree(retString, false)
      val params = methodParams(paramString)

      val throwsClause: List[UseTree] = if(exceptions != null) {
        exceptions.toList.map { e =>
          stringToUseTree(e, true)
        }
      } else Nil

      val meth = TreeFactories.mkMethodDef(mods, ret, Name(name), params,
        throwsClause, NoTree)
      members = meth::members
      null
    }

    override def visitEnd(): Unit = {

      val body = TreeFactories.mkTemplate(members)
      clazz = clazzFactory(body)
    }
  }
}

class ClassPathLoader(val classPaths: List[JFile])
  extends ClassPathLoaderApi
