package ch.usi.inf.l3.sana.tiny

import scopt.OptionParser
import java.util.logging.Level
import java.io.File

/* We preferred using object instead of package here,
   to make sure that nobody accidentally have access
   to the private setters in class SanaConfig, by simply
   naming their packages after this one.
 */
object settings {
  class SanaConfig {
    /**
      * @group Testing and Debugging
      */
    private[this] var printTrees_ : Option[String] = None
    def printTrees: Option[String] = this.printTrees_
    private[settings] def printTrees_=(v: Option[String]): Unit =
      this.printTrees_ = v
    /**
      * @group Testing and Debugging
      */
    private[this] var logLevel_ : Level = Level.SEVERE
    def logLevel: Level = this.logLevel_
    private[settings] def logLevel_=(l: Level): Unit =
      this.logLevel_ = l


    /**
      * @group Testing and Debugging
      */
    private[this] var isTest_ : Boolean = false
    def isTest: Boolean = this.isTest_
    private[settings] def isTest_=(v: Boolean): Unit =
      this.isTest_ = v

    /**
      * @group Testing and Debugging
      */
    private[this] var isVerbose_ : Boolean = false
    def isVerbose: Boolean = this.isVerbose_
    private[settings] def isVerbose_=(v: Boolean): Unit =
      this.isVerbose_ = v


    /**
      * @group Plugins
      */
    private[this] var plugins_ : Vector[String] = Vector()
    def plugins: Vector[String] = this.plugins_
    private[settings] def plugins_=(v: Vector[String]): Unit =
      this.plugins_ = v

    /**
      * @group Compilation Options
      */
    private[this] var classpath_ : Vector[String] = Vector()
    def classpath: Vector[String] = this.classpath_
    private[settings] def classpath_=(v: Vector[String]): Unit =
      this.classpath_ = v

    /**
      * @group Compilation Options
      */
    private[this] var files_ : List[String] = Nil
    def files: List[String] = {
      def allFiles(files: List[File], acc: List[String]): List[String] = {
        files match {
          case Nil                                => acc
          case (x::xs)        if x.isDirectory    =>
            val nestedFiles = allFiles(x.listFiles.toList, Nil)
            allFiles(xs, acc ++ nestedFiles)
          case (x::xs)                            =>
            val thisFile    = x.getCanonicalPath
            allFiles(xs, thisFile::acc)

        }
      }
      allFiles(this.files_.map(new File(_)), Nil)
    }
    private[settings] def files_=(v: List[String]): Unit =
      this.files_ = v

    /**
      * @group Compilation Options
      */
    private[this] var destination_ : Option[String] = None
    def destination: Option[String] = this.destination_
    private[settings] def destination_=(v: Option[String]): Unit =
      this.destination_ = v
  }


  class CommandLineArgumentParser(val config: SanaConfig,
                                  val langName: String,
                                  val langVersion: String,
                                  val fmName: String) {

    def parser = new OptionParser[Unit]("scopt") {
        head(langName, langVersion)
        opt[Unit]("Ytest") action { case _ =>
          config.isTest = true
        } text(s"To active testing mode for $fmName")
        opt[Unit]('v', "verbose") action { case _ =>
          config.isVerbose = true
        } text(s"Set verbose flag to $fmName")
        opt[Seq[String]]("XPlugin") action { case (plugins, _) =>
          config.plugins = config.plugins ++ plugins
        } valueName("<plugin1>,<plugin2>,...") text(
          "Comma seperated plugin names to be used.")
        opt[String]("Xlog") action { case (log, _) =>
          config.logLevel = log match{
            case "off"      => Level.OFF
            case "all"      => Level.ALL
            case "debug"    => Level.FINE
            case "info"     => Level.INFO
            case "warning"  => Level.WARNING
            case "severe"   => Level.SEVERE
          }
        } validate { x =>
          if (! List("off", "all", "debug", "info",
            "warning", "severe").contains(x))
            failure (s"Option $x wasn't understandable")
          else success
        }text("Set the logging level of Sana, possible options: \n" ++
          "off, all, debug, [severe].")
        opt[String]('d', "destination") action { case (dest, _) =>
          config.destination = Some(dest)
        } text(s"Set the destination directory for the compiled classes")
        arg[String]("<file>...") required() unbounded() action { case (f, _) =>
          config.files = config.files ++ List(f)
        } text("Unbounded filenames or paths to compile")
        opt[Seq[String]]("classpath") abbr("cp") action { case (cp, _) =>
          config.classpath = config.classpath ++ cp
          // config.copy(libName = k, maxCount = v)
        } valueName("<cp1>,<cp2>, ...") text(
          "Comma seperated classpath paths.")
        help("help") text("prints this usage text")
        // opt[Boolean]("-SPlugin") action { (x, c) =>
          // config.copy(foo = x) } text("To active testing mode for Sana")
      }
  }
}
