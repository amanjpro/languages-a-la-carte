package ch.usi.inf.l3.sana.modulej

import ch.usi.inf.l3.sana
import sana.tiny
import sana.modulej
import tiny.settings.{SanaConfig,CommandLineArgumentParser}
import tiny.errors.ErrorReporting
import tiny.debug.logger

object Main {
  def processOptions(args: Array[String],
                        ln: String,
                        lv: String,
                        fn: String): Either[String, SanaConfig] = {
        val config = new SanaConfig
        val processor = new CommandLineArgumentParser(config, ln, lv, fn)
        if(processor.parser.parse(args)) {
          Right(config)
        } else {
          Left("Bad set of arguments\n" ++ processor.parser.usage)
        }
      }

  def main(args: Array[String]): Unit = {


    val config = processOptions(args, langName, langVersion,
        tiny.frameworkName) match {
      case Right(config) => config
      case Left(msg)     =>
        println(msg)
        System.exit(1)
        ???  // To satisfy the type checker
    }

    val ln = langName
    val lv = langVersion
    logger.setLevel(config.logLevel)

    val compiler = new CompilerImpl(config)
    compiler.start
    ErrorReporting.isErroneous match {
      case true     =>
        ErrorReporting.errors.foreach(Console.err.println(_))
      case _        =>
        println("Compilation Successful")
    }

    // TODO: Here it should go to codegen
    // units.foreach(println)
  }
}
