package org.scalalabs.basic.lab01

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
/**
 * In this Lab you will implement a ScalaTest testcase.
 *
 * Instructions:
 * 1. Implement the divide method in Euro that has the following signature:  def /(divider:Int) = ???
 * - If the divider is <=0 throw an IllegalArgumentException
 *
 * 2. Write a ScalaTest using a Spec of your choice to test:
 * - Happy flow (divider is > 0)
 * - Alternative flow (divider is <= 0)
 */
@RunWith(classOf[JUnitRunner])
class ScalaTestExerciseTest extends FlatSpec with Matchers {
  "A Euro" should "correctly divide by a positive integer" in {
    val res = new Euro(1, 50) / 3
    res.euro should be (0)
    res.cents should be (50)
  }

  it should "throw an IllegalArgumentException for an integer less than 1" in {
    try {
      val res = new Euro(1, 50) / 0
      fail("Should have thrown Illegal Argument Exception")
    }
    catch {
      case e: IllegalArgumentException => println("illegal arg. exception");
    }

  }
}
