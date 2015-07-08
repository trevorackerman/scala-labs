package org.scalalabs.basic.lab01

import java.lang.{ IllegalArgumentException ⇒ IAE }
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

/**
 * Beginner Hello World:
 * Scala Objects
 * Scala inheritence with Traits
 * Scala Classes and companion Objects
 */
@RunWith(classOf[JUnitRunner])
class HelloWorldExerciseTest extends Specification {

  /*
  * Scala Objects
  *
  * Your job is to implement the objects and classes in
  * such a way that the tests in this suite all succeed.
  */
  "Scala Objects" should {
    "say hello" in {
      // The === operator used below is not an operator at all but a method in the
      // JUnitSuite super class, which is part of the ScalaTest library. it behaves
      // as a traditional assertEquals but produces very clear assertion errors when
      // values don't match. In Scala, methods can be used as if they were operators.
      HelloWorld.sayHello === "Hello from Scala"
    }
    "echo" in {
      HelloWorld.echo("Echo") === "Echo"
    }
  }
  /*==============================================================*/

  /*
  * Scala inheritence with Traits
  *
  * Your job is to implement the objects and classes in
  * such a way that the tests in this suite all succeed.
  *
  * Hint:
  * - combine the methods in HelloTrait and Worldtrait to
  *   create a new message
  */
  "Scala Traits" should {
    "say hello" in {
      HelloWorldWithTraits.hello === "Hello World"
    }
  }

  /*==============================================================*/

  /*
  * Scala Classes and companion Objects
  *
  * Your job is to implement the objects and classes in
  * such a way that the tests in this suite all succeed.
  *
  * Hint:
  * - A Class may have a companion Ojbect by the same name, defined
  *   in the same source file
  * - An Object can be constructed using an apply method
  */
  "Scala Companion Object" should {
    "serve as factory" in {
      val helloWorldInstance = HelloWorldClassAndObject("Hello")
      helloWorldInstance.echo === "Hello"
    }
  }

}
