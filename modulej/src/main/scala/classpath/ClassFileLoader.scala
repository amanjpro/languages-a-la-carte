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

trait ClassFileLoaderApi {

  protected def classPaths: List[JFile]
  private[this] lazy val urls: Array[URL] =
    classPaths.map(_.toURI.toURL).toArray
  private[this] lazy val classLoader: SanaClassLoader =
    new SanaClassLoader(urls)


  def loadClass(name: String): Tree = {
    val classData    = classLoader.getResourceAsStream(name)
    val cr           = new ClassReader(classData)
    val reader       = new ClassFileParser(name)
    cr.accept(reader, 0)
    val innerClasses = reader.innerClasses.map(loadClass(_))
    val body         =
      TreeFactories.mkTemplate(innerClasses ++ reader.clazz.body.members)
    val clazz        =
      TreeFactories.mkClassDef(reader.clazz.mods | COMPILED, reader.clazz.name,
              reader.clazz.parents, body)
    clazz.sourceName = reader.source
    def toPackages(pkgs: List[String], member: Tree): Tree = pkgs match {
      case Nil                             => member
      case (x::Nil)                        =>
        TreeFactories.mkPackageDef(Nil, Name(x), List(member))
      case (x::xs)                         =>
        val rest = toPackages(xs, member)
        TreeFactories.mkPackageDef(Nil, Name(x), List(rest))
    }

    toPackages(name.split("[.]").toList.dropRight(1), clazz)
  }


  class SanaClassLoader(classpath: Array[URL])
    extends URLClassLoader(classpath, null) {
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


    override def getResourceAsStream(name: String): InputStream = {
      findResource(name.replace('.', '/') + ".class").openStream
    }

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

  protected class ClassFileParser private(version: Int,
    val className: String) extends ClassVisitor(version) with Opcodes {

    def this(className: String) = this(Opcodes.ASM4, className)
    private[this] val bytecodeClassName = className.replace('.', '/')

    var clazz: ClassDefApi = _
    var clazzFactory: TemplateApi => ClassDefApi = _

    var members: List[DefTree] = Nil
    var source: String = _
    var innerClasses: List[String] = Nil

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
          val tuse        = TreeFactories.mkTypeUse(Name(tpe))
          dims.foldLeft(tuse: UseTree)((z, y) => {
            TreeFactories.mkArrayTypeUse(z)
          })
        case nme        =>
          val sig2 = if(classSig) nme
                     else nme.substring(1, nme.size -1)
          stringToUseTree(sig2.split("/").toList) match {
            case select@Select(qual, id: Ident)            =>
              val tuse = TreeFactories.mkTypeUse(id.name)
              TreeFactories.mkSelect(qual, tuse)
            case use                                       =>
              use
          }
      }
    }

    private def stringToUseTree(sig: List[String]): UseTree = sig match {
      case List(x)                =>
        TreeFactories.mkTypeUse(Name(x))
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
      val sName = name.replace('/', '.')
      if(! acc.isPrivateAcc && outerName == bytecodeClassName)
        innerClasses = sName::innerClasses
      else
        ()
    }


    override def visitField(access: Int, name: String, desc: String,
          signature: String, value: Object): FieldVisitor = {
      val acc = parseAccessFlag(access)
      val isFinal = parseFinalFlag(access)
      val isStatic = parseStaticFlag(access)
      val mods = acc | isFinal | isStatic | FIELD | COMPILED


      val tpt  = stringToUseTree(desc, false)
      val vdef =
        TreeFactories.mkValDef(mods, tpt, Name(name), NoTree)
      members = vdef::members
      null
    }

    override def visitMethod(access: Int, name: String, desc: String,
          signature: String, exceptions: Array[String]): MethodVisitor = {

      val acc           = parseAccessFlag(access)
      val isFinal       = parseFinalFlag(access)
      val isStatic      = parseStaticFlag(access)
      val isAbstract    = parseAbstractFlag(access)
      val isConstructor = if(name == CONSTRUCTOR_NAME.asString) {
        Flags(CONSTRUCTOR)
      } else noflags
      val mods          =
        acc | isFinal | isStatic | isAbstract | isConstructor | COMPILED


      val (paramString, retString) = {
        val endParamIndex = desc.indexOf(')')
        val (fst, snd)    = desc.splitAt(endParamIndex)
        (fst.substring(1), snd.substring(1))
      }

      val ret    = stringToUseTree(retString, false)
      val params = methodParams(paramString)

      val throwsClause: List[UseTree] = exceptions.toList.map { e =>
        stringToUseTree(e, true)
      }
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

class ClassFileLoader(val classPaths: List[JFile])
  extends ClassFileLoaderApi
