package org.scalalabs.basic.lab03

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import PatternMatchingExercise._
/**
 * @see PatternMatchingExercise
 */
@RunWith(classOf[JUnitRunner])
class PatternMatchingExerciseTest extends Specification {

  "PatternMatchingExercise" should {
    "match language on strings" in {
      describeLanguage("Java") === "OOP" 
      describeLanguage("Smalltalk") === "OOP" 
      describeLanguage("Clojure") === "Functional" 
      describeLanguage("Haskell") === "Functional" 
      describeLanguage("Scala") === "Hybrid" 
      describeLanguage("C") === "Procedural" 
      describeLanguage("Oz") === "Unknown" 
    }
    "match on input type" in {
      matchOnInputType("A String") === "A string with length 8" 
      matchOnInputType(10) === "A positive integer"
      matchOnInputType(Person("Jack", 39)) === "A person with name: Jack"
      matchOnInputType(1 to 11 toSeq) === "Seq with more than 10 elements"
      matchOnInputType(Seq("first", "second", "third", "fourth")) === "first: first, second: second, rest: List(third, fourth)"
      matchOnInputType(Some(1)) === "A Scala Option subtype"
      matchOnInputType(None) === "A Scala Option subtype"
      matchOnInputType(10l) === "Some Scala class"
      matchOnInputType(null) === "A null value"
    }
    "check age" in {
      older(new Person("Jack", 31)) === Some("Jack")
      older(new Person("Jack", 30)) === None
    }
    "match partial functions" in {
      //pf1 and pf2 are both partial functions.
      //These inherit from Scala's Function class, with an extra method: isDefinedAt
      //  pf3 should be defined in terms of pf1 and pf2

      pf1.isDefinedAt("scala-labs") must beTrue
      pf1.isDefinedAt("stuff") must beTrue
      pf1.isDefinedAt("other stuff") must beFalse

      pf2.isDefinedAt("other stuff") must beTrue

      pf3.isDefinedAt("scala-labs") must beTrue
      pf3.isDefinedAt("other stuff") must beTrue
    }
  }
}