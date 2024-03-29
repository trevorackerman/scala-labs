package org.scalalabs.basic.lab04
import ImplicitConversionExercise01._
import ImplicitConversionExercise01.Exercise01._
import ImplicitConversionExercise01.Exercise02._
import ImplicitConversionExercise01.Exercise03._
import ImplicitConversionExercise01.Exercise04._
import org.joda.time.Duration

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.joda.time._
/**
 * @see ImplicitConversionExercise01
 */
@RunWith(classOf[JUnitRunner])
class ImplicitConversionExercise01Test extends Specification with DeactivatedTimeConversions {

  "Exercise01" should {
    "convert string to list" in {
        stringToList("Hello") ==== List('H', 'e', 'l', 'l', 'o')
    }
  }

  "Exercise02" should {
    "convert celsius to fahrenheit" in {
            val c = new Celsius(10)
            val f = new Fahrenheit(30)
            TemperaturePrinter.printCelsius(c) ==== "It's 10.0 degree celsius"
            TemperaturePrinter.printCelsius(f) ==== "It's -1.11 degree celsius"
            TemperaturePrinter.printFahrenheit(c) ==== "It's 50.0 fahrenheit"
            TemperaturePrinter.printFahrenheit(f) ==== "It's 30.0 fahrenheit"
    }
  }

  "Exercise03" should {
    "add camelCase method to String" in {
            "camelCaseMe" ==== "camel case me".camelCase
    }


  }

  "Exercise04" should {
    "have a working time DSL" in {
            import TimeUtils._
            println(1 days)
            println((1 days) + (2 hours))
            (1 days).millis ==== new Duration(24L * 60L * 60L * 1000L).getMillis()
            (1.days + 2.hours).millis ==== new Duration(26L * 60L * 60L * 1000L).getMillis()
    }
  }
}

trait DeactivatedTimeConversions extends org.specs2.time.TimeConversions {
  override def intToRichLong(v: Int) = super.intToRichLong(v)
}