package org.scalalabs.basic.lab04
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.json4s._
import org.json4s.JsonDSL._
import Exercise01._
import Exercise02._
import Exercise03._
/**
 * @see ImplictConversionExercise02
 */
@RunWith(classOf[JUnitRunner])
class ImplicitConversionExercise02Test extends Specification {

  "Exercise01" should {
    "have a working money DSL" in {
      Euro(2, 0) must be_==~ (2 euros)
      Euro(0, 25) must be_==~ (25 cents)
      Euro(2, 25) must be_==~ (2 euros 25 cents)
    }
  }
  "Exercise02" should {
    "make Euro orderable without implementing the Ordered trait" in {
      val raw = Seq(Euro(2, 0), Euro(1, 1), Euro(1, 5))
      raw.sorted ==== Seq(Euro(1, 1), Euro(1, 5), Euro(2, 0))
    }
  }
  "Exercise03" should {
    import JsonConverter._
    val euro = Euro(1, 2)
    val json = ("symbol" -> "EUR") ~ ("amount" -> s"${euro.euros},${euro.cents}")
    "convert Euro to json" in {
      val out = convertToJson(euro)
      out ==== json
    }
    "convert json to Euro" in {
      val in = parseFromJson[Euro](json)
      euro === in
    }
  }
}

