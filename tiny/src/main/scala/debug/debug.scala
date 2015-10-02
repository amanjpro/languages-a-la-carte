package ch.usi.inf.l3.sana.tiny

import java.lang.{System => OS}
import java.io.File


package object debug {

  val logger: Logger = {
    val sp = OS.getProperty("file.separator")
    val commonPath = OS.getProperty("user.home") + sp + "." +
      frameworkName.toLowerCase
    val loggingDir = new File(commonPath)
    loggingDir.mkdirs
    val loggingPath = commonPath + sp + "logs.log"
    new Logger(loggingPath)
  }
}
