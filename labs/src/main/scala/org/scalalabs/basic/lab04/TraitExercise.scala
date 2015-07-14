package org.scalalabs.basic.lab04

/**
 * In this exercise you learn to isolate common behavior in traits.
 *
 * Beneath you see an implementation of a SimpleLogger.
 * This logger is used in the DummyService class in an intrusive manner,
 * directly referencing the logger implementation.
 *
 * To complete this exercise you have to provide a Loggable trait, that
 * contains all logging methods (debug and info). Replace the intrusive
 * implementation of SimpleLogger in the DummyService with this Loggable trait
 * so that the DummyService directly can use the the logging methods without
 * the need to create its own logger.
 */
object Level extends Enumeration {
  type Level = Value
  val Debug, Info = Value
}
import Level._

trait Loggable {
  var logConfig = Map(Debug -> false, Info -> true)

  var logHistory = Seq.empty[String]
  def clearHistory() = logHistory = Seq.empty[String]

  def debug(msg: => Any) = log(Debug, msg)
  def info(msg: => Any) = log(Info, msg)

  def log(level: Level, msg: ⇒ Any) = {
    def isLevelEnabled(level: Level) = logConfig.getOrElse(level, false)
    if (isLevelEnabled(level)) {
      val logMsg = f"$level%-7s $clazz $msg"
      logHistory = logHistory :+ logMsg
      println(logMsg)
    }
  }

  var clazz: String = getClass().getName()
}

class DummyService extends Loggable {
  def sendSomething(msg: Any) = {
    debug("Prepare sending")
    info(s"$msg successfully sent")
    debug("Done")
  }
}

