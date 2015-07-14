package org.scalalabs.basic.lab04
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import org.specs2.specification._
import Level._
@RunWith(classOf[JUnitRunner])
class TraitExerciseTest extends Specification {
  sequential
  val enableAllLevels = Map(Debug -> true, Info -> true)
  val disableAllLevels = Map(Debug -> false, Info -> false)
  val firstDebugStatement = "Debug   org.scalalabs.basic.lab04.DummyService Prepare sending"
  val infoStatement = (msg: String) ⇒ s"Info    org.scalalabs.basic.lab04.DummyService $msg successfully sent"

  val lastDebugStatement = "Debug   org.scalalabs.basic.lab04.DummyService Done"
  val service:DummyService = new DummyService()

  "Exercise 1: Logger Trait" should {

    "log all events" in new cleanLogger {
      val msg = "message"

      service.logConfig = enableAllLevels
      service.sendSomething(msg)

      service.logHistory.size === 3

      val first :: second :: third :: Nil = service.logHistory

      first === firstDebugStatement
      second === infoStatement(msg)
      third === lastDebugStatement
    }

    "not create log message in case level is not enabled" in new cleanLogger {
      service.logConfig = disableAllLevels
      var longStringCreated = ""
      def createLongString = {
        longStringCreated = "Scala " * 1000000
        longStringCreated
      }
      val impl = new AnyRef with Loggable
      impl.debug(createLongString)

      service.logHistory must beEmpty
      longStringCreated ==== ""
    }
  }
  trait cleanLogger extends Scope {
    service.clearHistory()
  }
}
