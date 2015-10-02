package ch.usi.inf.l3.sana.tiny.debug

import java.util.logging.{Logger => JLogger, Level,
  FileHandler, SimpleFormatter}


class Logger(destination: String) {
  private[this] val logger = JLogger.getLogger(JLogger.GLOBAL_LOGGER_NAME)
  private[this] val handler = new FileHandler(destination)
  private[this] val formatter = new SimpleFormatter

  handler.setFormatter(formatter)
  logger.addHandler(handler)


  def debug(msg: String): Unit = {
    logger.log(Level.FINE, msg)
  }

  def info(msg: String): Unit = {
    logger.log(Level.INFO, msg)
  }
  def warning(msg: String): Unit = {
    logger.log(Level.WARNING, msg)
  }
  def severe(msg: String): Unit = {
    logger.severe(msg)
  }

  def setLevel(level: Level): Unit = {
    handler.setLevel(level)
    logger.setLevel(level)
  }
}

