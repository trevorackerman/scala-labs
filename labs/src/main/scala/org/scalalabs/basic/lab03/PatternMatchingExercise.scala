package org.scalalabs.basic.lab03
import sys._
/**
 * This exercise introduces you to the powerful pattern matching features of Scala.
 *
 * Pattern matching can in its essence be compared to Java's 'switch' statement,
 * even though it provides many more possibilites. Whereas the Java switch statmenet
 * lets you 'match' primitive types up to int's, Scala's pattern matching goes much
 * further. Practically everything from all types of objects and Collections
 * can be matched, not forgetting xml and a special type of class called case classes.
 *
 * Pattern matching is also often used in combination with recursive algorithms.
 *
 * For this exercise exclusively use pattern matching constructs in order to make the
 * corresponding unit test work.
 *
 * Reference material to solve these exercises can be found here:
 * Pattern matching in general: http://programming-scala.labs.oreilly.com/ch03.html#PatternMatching
 * Pattern matching in combination with partial functions: http://programming-scala.labs.oreilly.com/ch08.html#PartialFunctions
 */

object PatternMatchingExercise {

  /**
   * ***********************************************************************
   *  pattern matching exercises
   * For expected solution see unittest @PatternMatchingExerciseTest
   * ***********************************************************************
   */

  def describeLanguage(s: String) = {
    s match {
      case _ if (s == "Java" || s == "Smalltalk") => "OOP"
      case _ if (s == "Clojure" || s == "Haskell") => "Functional"
      case "Scala" => "Hybrid"
      case "C" => "Procedural"
      case _ => "Unknown"
    }
  }

  def matchOnInputType(in: Any):String = {
    in match {
      case s:String => "A string with length " + s.length
      case i:Int => "A " + (if (i >= 0) "positive" else "negative") + " integer"
      case p:Person => "A person with name: " + p.name
      case seq:Seq[Any] =>
        if (seq.size > 10) {
          "Seq with " + (if (seq.size > 10) "more" else "less") + " than 10 elements"
        }
        else {
          s"first: ${seq(0)}, second: ${seq(1)}, rest: ${seq.drop(2)}"
        }
      case o:Option[Any] => "A Scala Option subtype"
      case c:AnyRef => "Some Scala class"
      case null => "A null value"
    }
  }

  def older(p: Person): Option[String] = {
    p match {
      case _ if (p.age > 30) => Some(p.name)
      case _ => None
    }
  }

  /**
   * ***********************************************************************
   * Pattern matching with partial functions
   * For expected solution see @PatternMatchingExerciseTest
   * ***********************************************************************
   */

  val pf1: PartialFunction[String, String] = {
    case s: String if s.startsWith("other") == false => s
  }

  val pf2: PartialFunction[String, String] = {
    case s: String if s.startsWith("other") => s
  }

  val pf3: PartialFunction[String, String] = {
    case s:String if (pf1.isDefinedAt(s) || pf2.isDefinedAt(s)) => s
  }

}

case class Person(name: String, age: Int)